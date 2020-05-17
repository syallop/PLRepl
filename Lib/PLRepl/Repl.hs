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
import Data.Maybe
import Data.List (intercalate)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (Sum,Product)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega

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
type Print o = Text -> o -> Maybe (ExprFor CommentedPhase, TypeFor CommentedPhase) -> Repl o Doc

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
  , _print       = \_ _ _ -> replError $ EMsg $ text "No print function defined in replConfig"
  }

-- | The current st of the repl is a consistent view of type and expression bindings.
data ReplState o = ReplState
  { _replConfig   :: ReplConfig o
  , _typeCheckCtx :: TypeCheckCtx
  , _exprStore    :: HashStore o        -- Exprs can be stored by their hash
  }

-- SomeReplState is a ReplState where the type of Grammar has been forgotten.
data SomeReplState = forall o. SomeReplState (ReplState o)

-- | An initial, empty replst
emptyReplState
  :: ReplState o
emptyReplState = ReplState
  { _replConfig   = emptyReplConfig
  , _typeCheckCtx = topTypeCheckCtx mempty
  , _exprStore    = error "No HashStore defined"
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
  case reduce underTypeCtx initialExpr of
    Left err   -> replError err
    Right expr -> pure expr

-- A simple Eval function which takes a plain Expr, type checks it
-- and then reduces.
replEvalSimple
  :: Eval CommentedExpr
replEvalSimple commentedExpr = do
  let expr = stripComments commentedExpr
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

replStore
  :: HashAlgorithm
  -> o
  -> Repl o (StoreResult o, Hash)
replStore alg o = Repl $ \replState -> do
  let exprStore = _exprStore replState
  mRes <- storeByHash (_exprStore replState) alg o
  pure $ case mRes of
    Nothing
      -> (replState, Left . EMsg . text $ "Failed to store expression in store")

    Just (exprStore', res, hashKey)
      -> (replState{_exprStore = exprStore'}, Right (res, hashKey))

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
replPrint originalTxt a mEvaluated = Repl $ \replState ->
  let printF = _print . _replConfig $ replState
      Repl replF = printF originalTxt a mEvaluated
   in replF replState

-- Read, Eval and return the text to be printed.
-- Drive this function with input, do something with the output and loop for a
-- REPL.
replStep
  :: Text
  -> Repl o Doc
replStep input = do
  parsedOutput <- replRead input
  mEvaluated   <- replEval parsedOutput

  replStore SHA512 parsedOutput
  replPrint input parsedOutput (fmap (\(expr,typ) -> (addComments expr,addTypeComments typ)) mEvaluated)

