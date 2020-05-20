{-# LANGUAGE
    RankNTypes
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , UndecidableInstances
  #-}
{-|
Module      : PLRepl.Repl
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

PLRepl.Repl abstracts the Read Eval Print Loop for some PL repl.
It accepts a repl configuration which it understands how to drive.

Repl configuration(s) are found under PLRepl.Repl.* This abstraction allows
defining multiple repls which can be switched between, for example we might
want a repl for different expression grammars or a repl exclusively for type
signatures.

|-}
module PLRepl.Repl
  ( Read
  , Eval
  , Print
  , PrintArguments (..)
  , ReplConfig (..)
  , ReplState (..)
  , emptyReplState
  , Repl ()
  , _unRepl

  -- Core API.
  , replStep
  , replRead
  , replEval
  , replPrint

  -- Convenience functions
  , replGrammar
  , replError
  , replTypeCheck
  , replReduce
  , replEvalSimple

  , SomeReplState (..)
  , SomeReplConfig (..)
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Expr
import PL.ExprLike
import PL.Kind
import PL.Var
import PL.Name
import PL.Reduce
import PL.Hash
import PL.HashStore
import PL.TypeCheck
import PL.CodeStore
import PL.Store
import PL.TyVar
import PL.Type hiding (parens)
import PL.Type.Eq
import PL.TypeCtx
import PL.Pattern
import PLLispy
import qualified PLParser as PLParser

import PLParser
import PLPrinter
import PLGrammar
import Reversible

import Control.Applicative
import Control.Monad (ap)
import Data.Foldable
import Data.Maybe
import Data.List (intercalate,intersperse)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (Sum,Product)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import Data.Map (Map)

import Prelude hiding (Read)

-- | A Read function takes a Grammar on 'o' and attempts to parse text into an
-- 'o'.
type Read o = Text -> Repl o o

-- | An Eval function transforms some value 'o'. Into a Repl function which may
-- succeed with a new expression along with its type.
-- A return value of Nothing is still a success, just with no new expression.
type Eval o = o -> Repl o (Maybe (ExprFor DefaultPhase,TypeFor DefaultPhase))

-- | A Print function takes the initial text, the parsed thing 'o', a possible expression and type
-- it reduced to and returns some output Text to print.
type Print o = PrintArguments o -> Repl o Doc

data PrintArguments o = PrintArguments
  { _readText  :: Text
  , _parsed    :: Maybe o
  , _evaluatedExpr :: Maybe (ExprFor CommentedPhase)
  , _evaluatedType :: Maybe (TypeFor CommentedPhase)

  , _storedExpr        :: Maybe (StoreResult Expr, Hash)
  , _storedType        :: Maybe (StoreResult Type, Hash)
  , _storedExprHasType :: Maybe (StoreResult Hash)
  }

-- | A ReplConfig is a set of active Grammar alongside Read, Eval and Print
-- functions defined upon it.
data ReplConfig o where
  ReplConfig
    :: { _someGrammar :: Grammar o -- A Grammar to read.
       , _read        :: Read  o
       , _eval        :: Eval  o
       , _print       :: Print o
       }
    -> ReplConfig o

-- SomeReplConfig is a ReplConfig where the type of Grammar has been forgotten.
data SomeReplConfig = forall o. SomeReplConfig (ReplConfig o)

-- | The empty ReplConfig always fails and outputs error documents for Read Eval
-- and Print.
emptyReplConfig
  :: ReplConfig o
emptyReplConfig = ReplConfig
  { _someGrammar = rempty -- The Grammar that always fails.
  , _read        = \_ -> replError $ EMsg $ text "No read function defined in replConfig"
  , _eval        = \_ -> replError $ EMsg $ text "No eval function defined in replConfig"
  , _print       = \_ -> replError $ EMsg $ text "No print function defined in replConfig"
  }

-- | The current st of the repl is a consistent view of type and expression bindings.
data ReplState o = ReplState
  { _replConfig   :: ReplConfig o
  , _typeCheckCtx :: TypeCheckCtx
  , _codeStore    :: CodeStore
  }

-- SomeReplState is a ReplState where the type of Grammar has been forgotten.
data SomeReplState = forall o. SomeReplState (ReplState o)

-- | An initial, empty replst
emptyReplState
  :: ReplState o
emptyReplState = ReplState
  { _replConfig   = emptyReplConfig
  , _typeCheckCtx = topTypeCheckCtx mempty
  , _codeStore    = error "No CodeStore defined"
  }

-- | A Repl has replst as state which it always returns alongside a successful
-- result or an error.
--
-- This means, for example, we can update our state and throw an error at the
-- same time. One usecase would be tracking line numbers of entered expressions, valid or not.
--
-- 'o' is the output type the grammar specifies.
-- 'a' is the final result type.
newtype Repl o a = Repl
  {_unRepl :: ReplState o -> IO (ReplState o, Either (Error Expr Type Pattern TypeCtx) a)}

instance Functor (Repl o) where
  fmap f (Repl r) = Repl $ \st -> do (st',res) <- r st
                                     pure (st', case res of
                                                  Left err -> Left err
                                                  Right a  -> Right $ f a)

instance Applicative (Repl o) where
  pure = return
  (<*>) = ap


instance Monad (Repl o) where
  return a = Repl $ \st -> pure (st,Right a)

  (Repl f) >>= fab = Repl $ \st -> do (st',res) <- f st
                                      case res of
                                          Left err -> pure (st',Left err)
                                          Right a  -> let Repl g = fab a
                                                         in g st'

-- | Inject an error into the repl
replError
  :: Error Expr Type Pattern TypeCtx
  -> Repl o x
replError err = Repl $ \st -> pure (st,Left err)

-- Resolving checks every top-level referenced expression:
-- - Has an associated type hash
-- - Associated type has has an associated type
--
-- And if so, adds each type association to the typechecking context.
-- Returns the type associations that were successfully resolved and introduced.
replResolveExprsTypes
  :: Expr
  -> Repl o (Map ContentName Type)
replResolveExprsTypes expr = Repl $ \st -> do
  -- TODO: Break this logic out into individual operations, consider migrating to CodeStore

  let codeStore = _codeStore st
  let referencedNames = gatherNames expr

  -- For each immediately referenced name, query for its type hash,
  -- accumulating:
  -- - The resulting codestore
  -- - Any expressions which could not be resolved
  -- - A mapping of expression hashes to type hashes for things which could be
  --   resolved.
  (codeStore',unresolved,resolved)
    <- foldrM (\exprHash (codeStore, accUnresolved, accResolved)
                 -> do mRes <- lookupExprType codeStore exprHash
                       pure $ case mRes of
                         Nothing
                           -> (codeStore, Set.insert exprHash accUnresolved, accResolved)
                         Just (codeStore', typeHash)
                           -> (codeStore, accUnresolved, Map.insert exprHash typeHash accResolved)
              )
              (codeStore, Set.empty, Map.empty)
              (fmap contentName . Set.toList $ referencedNames)

  -- If we have any unresolved, stop and report an error
  if not $ Set.null unresolved
    then pure (st{_codeStore = codeStore'}
              ,Left . EMsg . mconcat $
                [ text "Could not resolve referenced expressions to hashes:"
                , lineBreak
                , mconcat . intersperse lineBreak . map (text . showBase58) . Set.toList $ unresolved
                , lineBreak
                , text "After gathering names:"
                , lineBreak
                , mconcat . intersperse lineBreak . map document . Set.toList $ referencedNames
                , lineBreak, lineBreak
                ]
              )

    -- All expression hashes had an associated type hash.
    -- Now resolve each type hash to a type.
    -- Note we're not:
    -- - Checking the expr hashes resolve
    -- - Validating the expression actually has the type the codebase claims.
    else do (codeStore'',unresolvedTypes,resolvedTypes)
              <- foldrM (\(exprHash,typeHash) (codeStore, accUnresolved, accResolved)
                          -> do mRes <- lookupType codeStore typeHash
                                pure $ case mRes of
                                  Nothing
                                    -> (codeStore, Set.insert (exprHash,typeHash) accUnresolved, accResolved)

                                  Just (codeStore', typ)
                                    -> (codeStore', accUnresolved, Map.insert exprHash typ accResolved)
                        )
                        (codeStore', Set.empty, Map.empty)
                        (Map.toList resolved)

            -- If any type hashes did not resolve, stop and report an error.
            if not $ Set.null unresolved
              then pure (st{_codeStore = codeStore''}
                        ,Left . EMsg . mconcat $
                          [ text "All referenced expressions resolved to a type hash however some type hashes did not resolve to a type:"
                          , lineBreak
                          , string . show $ unresolvedTypes
                          ]
                        )
              else let typeCheckCtx  = _typeCheckCtx st
                       typeMappings  = _contentHasType typeCheckCtx

                       newMappings = Map.mapKeys mkContentName resolvedTypes

                       typeMappings' = Map.union newMappings typeMappings
                       typeCheckCtx' = typeCheckCtx{_contentHasType = typeMappings'}
                    in pure (st{_codeStore    = codeStore''
                               ,_typeCheckCtx = typeCheckCtx'
                               }
                            ,Right newMappings
                            )

  -- For each name, query the CodeStore for the associated type
  -- Add each associated type to the type context

-- Type check an expression in the repl context.
replTypeCheck
  :: Expr
  -> Repl o Type
replTypeCheck expr = Repl $ \st -> pure $
  case exprType (_typeCheckCtx st) expr of
    Left err
      -> (st,Left err)

    Right ty
      -> (st,Right ty)

-- Get the type context the repl is using to parse/ evaluate types.
replTypeCtx
  :: Repl o TypeCtx
replTypeCtx = Repl $ \st -> pure (st, Right . _typeCtx . _typeCheckCtx $ st)

-- Reduce an expression in the repl context.
replReduce
  :: Expr
  -> Repl o Expr
replReduce initialExpr = do
  underTypeCtx <- replTypeCtx
  case reduce (topReductionCtx underTypeCtx) initialExpr of
    Left err   -> replError err
    Right expr -> pure expr

-- A simple Eval function which takes a plain Expr, type checks it
-- and then reduces.
replEvalSimple
  :: Eval CommentedExpr
replEvalSimple commentedExpr = do
  let expr = stripComments commentedExpr

  replResolveExprsTypes expr

  ty      <- replTypeCheck expr
  redExpr <- replReduce    expr
  pure $ Just (redExpr,ty)

-- | Feed read text into the Repls configured read function.
replRead
  :: Read o
replRead input = Repl $ \replState ->
  let readF      = _read . _replConfig $ replState
      Repl replF = readF input
    in replF replState

replEval
  :: Eval o
replEval a = Repl $ \replState ->
  let evalF = _eval . _replConfig $ replState
      Repl replF = evalF a
   in replF replState

-- Store an expression, it's type and cache the relation that the expression has
-- been checked to have the type - this should be true if you don't want bad
-- things to happen.
replStore
  :: (Expr,Type)
  -> Repl o ((StoreResult Expr,Hash)
            ,(StoreResult Type,Hash)
            ,StoreResult Hash
            )
replStore (expr,typ) = (,,) <$> replStoreExpr expr
                            <*> replStoreType typ
                            <*> replStoreExprHasType (expr,typ)

-- Store an expression in the codestore
replStoreExpr
  :: Expr
  -> Repl o (StoreResult Expr, Hash)
replStoreExpr expr = Repl $ \replState -> do
  let codeStore = _codeStore replState
  mStoreExpr <- storeExpr codeStore expr
  pure $ case mStoreExpr of
    Nothing
      -> (replState, Left . EMsg . text $ "Failed to store expression")

    Just (codeStore', exprRes, exprHash)
      -> (replState{_codeStore = codeStore'}, Right (exprRes,exprHash))

-- Store a type in the codestore
replStoreType
  :: Type
  -> Repl o (StoreResult Type, Hash)
replStoreType typ = Repl $ \replState -> do
  let codeStore = _codeStore replState
  mStoreType <- storeType codeStore typ
  pure $ case mStoreType of
    Nothing
      -> (replState, Left . EMsg . text $ "Failed to store type")

    Just (codeStore', typeRes, typeHash)
      -> (replState{_codeStore = codeStore'}, Right (typeRes,typeHash))

-- Store the relation that an expression has been checked to have a type.
-- You should ensure this is true.
--
-- The result may contain the Hash of the replacement type if the type has
-- somehow changed.
replStoreExprHasType
  :: (Expr,Type)
  -> Repl o (StoreResult Hash)
replStoreExprHasType (expr,typ) = Repl $ \replState -> do
  let codeStore = _codeStore replState
  mStoreExprType <- storeExprHasType codeStore (hash expr,hash typ)
  pure $ case mStoreExprType of
    Nothing
      -> (replState, Left . EMsg . text $ "Failed to associate expression with type")

    Just (codeStore', exprTypeRes)
      -> (replState{_codeStore = codeStore'}, Right exprTypeRes)

-- | Acquire the Grammar the Repl is using to parse/ print 'o's.
replGrammar
  :: Repl o (Grammar o)
replGrammar = Repl $ \replState -> pure (replState, Right . _someGrammar . _replConfig $ replState)

-- | Defer to the print function to print:
-- - The original text supplied by the user
-- - A parsed thing
-- - A possible expression and type it reduced to.
replPrint
  :: Print o
replPrint printArgs = Repl $ \replState ->
  let printF = _print . _replConfig $ replState
      Repl replF = printF printArgs
   in replF replState

-- Read, Eval and return the text to be printed.
-- Drive this function with input, do something with the output and loop for a
-- REPL.
replStep
  :: Text
  -> Repl o Doc
replStep input = do
  let printArguments = PrintArguments
        { _readText      = input
        , _parsed        = Nothing
        , _evaluatedExpr = Nothing
        , _evaluatedType = Nothing
        , _storedExpr    = Nothing
        , _storedType    = Nothing
        , _storedExprHasType = Nothing
        }

  parsedOutput <- replRead input

  mEvaluated <- replEval parsedOutput
  case mEvaluated of
    Nothing
      -> replPrint printArguments
           {_parsed = Just parsedOutput
           }

    Just (evalExpr,evalType)
      -> do (storedExpr, storedType, storedExprHasType) <- replStore (evalExpr,evalType)
            replPrint printArguments
              { _parsed            = Just parsedOutput
              , _evaluatedExpr     = Just $ addComments evalExpr
              , _evaluatedType     = Just $ addTypeComments evalType
              , _storedExpr        = Just storedExpr
              , _storedType        = Just storedType
              , _storedExprHasType = Just storedExprHasType
              }

