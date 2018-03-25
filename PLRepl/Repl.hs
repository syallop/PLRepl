{-# LANGUAGE
    RankNTypes
  , FlexibleContexts
  , OverloadedStrings
  , UndecidableInstances
  #-}
module PLRepl.Repl
  ( ReplState (..)
  , emptyReplState
  , Repl (..)
  , replError
  , replRead
  , replTypeCheck
  , replReduce
  , replEval
  , replPrint
  , replStep

  , plGrammarParser
  , megaparsecGrammarParser
  )
  where

import PL.Abstracts
import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.ExprLike
import PL.Kind
import PL.Grammar.Lispy
import PL.Grammar
import qualified PL.Grammar    as PL
import qualified PLParser as PLParser
import qualified PL.Megaparsec as PLMega
import PL.Name
import PL.Reduce
import PL.Type hiding (parens)
import PL.Type.Eq
import PL.TypeCtx

import PLParser
import PLPrinter
import PLGrammar

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

-- | The current st of the repl is a consistent view of type and expression bindings.
data ReplState b tb = ReplState
  {_exprBindCtx  :: ExprBindCtx b tb -- Expr bindings 'b' have types
  ,_typeBindCtx  :: TypeBindCtx tb   -- Type bindings have kinds

  ,_typeBindings :: TypeBindings tb -- Type bindings may have a bound or unbound type

  ,_typeCtx      :: TypeCtx tb      -- Names can be given to types
  }

instance
  ( Document b
  , Document tb
  , Document (TypeCtx tb)
  , Binds b (Type tb)
  , Binds tb Kind
  ) => Document (ReplState b tb) where
    document (ReplState exprBindCtx typeBindCtx typeBindings typeCtx) = mconcat
      [ document exprBindCtx
      , lineBreak
      , document typeBindCtx
      , lineBreak
      , document typeBindings
      , lineBreak
      , document typeCtx
      ]

-- | An initial, empty replst
emptyReplState
  :: (Binds b (Type tb)
     ,Binds tb Kind
     )
  => ReplState b tb
emptyReplState = ReplState
  {_exprBindCtx  = emptyCtx
  ,_typeBindCtx  = emptyCtx

  ,_typeBindings = emptyBindings

  ,_typeCtx     = mempty
  }

-- | A Repl has replst as state which it always returns alongside a successful
-- result or an error.
--
-- This means, for example, we can update our state and throw an error at the
-- same time. One usecase would be tracking line numbers of entered expressions, valid or not.
newtype Repl b abs tb a = Repl
  {_unRepl :: ReplState b tb -> (ReplState b tb, Either (Error tb) a)}

instance Functor (Repl b abs tb) where
  fmap f (Repl r) = Repl $ \st -> let (st',res) = r st
                                    in (st', case res of
                                                Left err -> Left err
                                                Right a  -> Right $ f a)

instance Applicative (Repl b abs tb) where
  pure = return
  (<*>) = ap


instance Monad (Repl b abs tb) where
  return a = Repl $ \st -> (st,Right a)

  (Repl f) >>= fab = Repl $ \st -> let (st',res) = f st
                                     in case res of
                                          Left err -> (st',Left err)
                                          Right a  -> let Repl g = fab a
                                                         in g st'

-- | Inject an error into the repl
replError
  :: Error tb
  -> Repl b abs tb r
replError err = Repl $ \st -> (st,Left err)

-- Read text and parse it into an expr when supplied Grammars for bindings,
-- abstractions and type bindings and the means to convert these grammars into
-- parsing functions.
replRead
  :: ( Ord tb
     , Eq b
     , Eq abs
     , Document b
     , Document abs
     , Document tb
     , Constraints b abs tb
     , Show b
     , Show abs
     , Show tb
     )
  => (forall a. Document a => Grammar a -> Text -> Repl b abs tb a) -- ^ Convert a Grammar to a parser
  -> Grammar b                                        -- ^ Expression bindings (E.G. Var)
  -> Grammar abs                                      -- ^ Expression abstraction (E.G. Type)
  -> Grammar tb                                       -- ^ Type bindings (E.G. Var)
  -> Text                                             -- ^ Input text
  -> Repl b abs tb (Expr b abs tb)
replRead grammarParser b abs tb = grammarParser $ expr b abs tb

-- Convert a Grammar to a parser using PLParser.
plGrammarParser
  :: Document a
  => Grammar a
  -> Text
  -> Repl b abs tb a
plGrammarParser grammar =
  let plParser = PL.toParser grammar
   in \txt -> case PLParser.runParser plParser txt of
                f@(ParseFailure expected cursor)
                  -> replError . EMsg . document $ f

                s@(ParseSuccess expr cursor)
                  | Text.null $ remainder cursor
                   -> pure expr

                  | otherwise
                   -> replError $ EMsg $ text "Parse succeeded but there were trailing characters: " <> document cursor

-- Convert a Grammar to a parser using Megaparsec.
megaparsecGrammarParser
  :: Grammar a
  -> Text
  -> Repl b abs tb a
megaparsecGrammarParser grammar =
  let megaparsecParser = PLMega.toParser grammar
   in \txt -> case Mega.runParser megaparsecParser "" txt of
                Left err
                  -> replError . EMsg . document $ err

                Right expr
                  -> pure expr

instance (Ord t, Mega.ShowToken t, Mega.ShowErrorComponent e) => Document (Mega.ParseError t e) where
  document = text . Text.pack . Mega.parseErrorPretty

-- Type check an expression in the repl context.
replTypeCheck
  :: ( Abstracts abs tb
     , Binds b (Type tb)
     , Binds tb Kind
     , Ord tb
     , Document b
     , Document abs
     , Document tb
     )
  => Expr b abs tb
  -> Repl b abs tb (Type tb)
replTypeCheck expr = Repl $ \st ->
  case exprType (_exprBindCtx st)
                (_typeBindCtx st)
                (_typeBindings st)
                (_typeCtx st) expr of
    Left err
      -> (st,Left err)

    Right ty
      -> (st,Right ty)

-- Reduce an expression in the repl context.
replReduce
  :: ( Binds b (Type tb)
     , Abstracts abs tb
     , Eq b
     )
  => Expr b abs tb
  -> Repl b abs tb (Expr b abs tb)
replReduce initialExpr = case reduce initialExpr of
  Left err   -> replError err
  Right expr -> pure expr

-- Type check and reduce an expression
replEval
  :: ( Binds b (Type tb)
     , Binds tb Kind
     , Abstracts abs tb
     , Eq b
     , Ord tb
     , Document b
     , Document abs
     , Document tb
     )
  => Expr b abs tb
  -> Repl b abs tb (Expr b abs tb,Type tb)
replEval expr = do
  ty      <- replTypeCheck expr
  redExpr <- replReduce    expr
  pure (redExpr,ty)

-- Transform a parsed expr, its reduction and type into some output to print
replPrint
  :: ( Document b
     , Document abs
     , Document tb
     , Implicits b abs tb
     , Ord tb
     , Eq b
     , Eq abs
     , Show b
     , Show abs
     , Show tb
     )
  => (Expr b abs tb,Expr b abs tb,Type tb)
  -> Repl b abs tb Text
replPrint (inputExpr,redExpr,ty) = pure . render . mconcat $
  [ text "input expression:"
  , lineBreak
  , fromMaybe mempty $ pprint (toPrinter exprI) inputExpr

  , text "reduces to:"
  , lineBreak

  , fromMaybe mempty $ pprint (toPrinter exprI) redExpr
  , lineBreak

  , text "with type:"
  , lineBreak

  , fromMaybe mempty $ pprint (toPrinter typI) ty
  , lineBreak
  ]

-- Produce the next context and parse, type-check and reduce or error.
replStep
  :: ( Ord tb
     , Document b
     , Document abs
     , Document tb
     , Implicits b abs tb
     , Binds b (Type tb)
     , Binds tb Kind
     , Abstracts abs tb
     , Eq b
     , Show b
     , Show abs
     , Show tb
     )
  => (forall a. Document a => Grammar a -> Text -> Repl b abs tb a) -- ^ Convert a Grammar to a parser
  -> Grammar b
  -> Grammar abs
  -> Grammar tb
  -> Text
  -> Repl b abs tb Text
replStep grammarParser b abs tb txt = do
  expr         <- replRead grammarParser b abs tb txt
  (redExpr,ty) <- replEval expr
  replPrint (expr,redExpr,ty)

