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
  , replError
  , replTypeCheck
  , replReduce
  , replEvalSimple

  , SomeReplState (..)
  , SomeReplConfig (..)
  )
  where

import PL.Abstracts
import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.ExprLike
import PLLispy
import PL.Kind
import PL.Name
import PL.Reduce
import PL.Type hiding (parens)
import PL.Type.Eq
import PL.TypeCtx
import qualified PL.Megaparsec as PLMega
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
type Read b abs tb o = Document o => Grammar o -> Text -> Repl b abs tb o o

-- | An Eval function transforms some value 'o'. Into a Repl function which may
-- succeed with a new expression along with its type.
-- A return value of Nothing is still a success, just with no new expression.
type Eval b abs tb o = o -> Repl b abs tb o (Maybe (Expr b abs tb,Type tb))

-- | A Print function takes the initial text, the parsed thing 'o', a possible expression and type
-- it reduced to and returns some output Text to print.
type Print b abs tb o = Document o => Text -> o -> Maybe (Expr b abs tb, Type tb) -> Repl b abs tb o Doc

-- | A ReplConfig is a set of active Grammar alongside Read, Eval and Print
-- functions defined upon it.
data ReplConfig b abs tb o where
  ReplConfig
    :: { _someGrammar :: Document o => Grammar o -- A Grammar to read.
       , _read        :: Read  b abs tb o
       , _eval        :: Eval  b abs tb o
       , _print       :: Print b abs tb o
       }
    -> ReplConfig b abs tb o

-- SomeReplConfig is a ReplConfig where the type of Grammar has been forgotten.
data SomeReplConfig b abs tb = forall o. Document o => SomeReplConfig (ReplConfig b abs tb o)

-- | The empty ReplConfig always fails and outputs error documents for Read Eval
-- and Print.
emptyReplConfig
  :: ReplConfig b abs tb o
emptyReplConfig = ReplConfig
  { _someGrammar = rempty -- The Grammar that always fails.
  , _read        = \_ _ -> replError $ EMsg $ text "No read function defined in replConfig"
  , _eval        = \_   -> replError $ EMsg $ text "No eval function defined in replConfig"
  , _print       = \_ _ _ -> replError $ EMsg $ text "No print function defined in replConfig"
  }

-- | The current st of the repl is a consistent view of type and expression bindings.
data ReplState b abs tb o = ReplState
  { _replConfig   :: ReplConfig b abs tb o

  , _exprBindCtx  :: ExprBindCtx b tb -- Expr bindings 'b' have types
  , _typeBindCtx  :: TypeBindCtx tb   -- Type bindings have kinds

  , _typeBindings :: TypeBindings tb -- Type bindings may have a bound or unbound type

  , _typeCtx      :: TypeCtx tb      -- Names can be given to types
  }

-- SomeReplState is a ReplState where the type of Grammar has been forgotten.
data SomeReplState b abs tb = forall o. Document o => SomeReplState (ReplState b abs tb o)

instance
  ( Document b
  , Document tb
  , Document (ExprBindCtx b tb)
  , Document (TypeBindings tb)
  , Document (TypeCtx tb)
  , Binds b (Type tb)
  , Binds tb Kind
  ) => Document (ReplState b abs tb o) where
    document (ReplState replConfig exprBindCtx typeBindCtx typeBindings typeCtx) = mconcat
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
  => ReplState b abs tb o
emptyReplState = ReplState
  { _replConfig   = emptyReplConfig
  , _exprBindCtx  = emptyCtx
  , _typeBindCtx  = emptyCtx

  , _typeBindings = emptyBindings

  , _typeCtx     = mempty
  }

-- | A Repl has replst as state which it always returns alongside a successful
-- result or an error.
--
-- This means, for example, we can update our state and throw an error at the
-- same time. One usecase would be tracking line numbers of entered expressions, valid or not.
--
-- 'o' is the output type the grammar specifies.
-- 'a' is the final result type.
newtype Repl b abs tb o a = Repl
  {_unRepl :: ReplState b abs tb o -> (ReplState b abs tb o, Either (Error tb) a)}

instance Functor (Repl b abs tb o) where
  fmap f (Repl r) = Repl $ \st -> let (st',res) = r st
                                    in (st', case res of
                                                Left err -> Left err
                                                Right a  -> Right $ f a)

instance Applicative (Repl b abs tb o) where
  pure = return
  (<*>) = ap


instance Monad (Repl b abs tb o) where
  return a = Repl $ \st -> (st,Right a)

  (Repl f) >>= fab = Repl $ \st -> let (st',res) = f st
                                     in case res of
                                          Left err -> (st',Left err)
                                          Right a  -> let Repl g = fab a
                                                         in g st'

-- | Inject an error into the repl
replError
  :: Error tb
  -> Repl b abs tb o x
replError err = Repl $ \st -> (st,Left err)

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
  -> Repl b abs tb o (Type tb)
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
  -> Repl b abs tb o (Expr b abs tb)
replReduce initialExpr = case reduce initialExpr of
  Left err   -> replError err
  Right expr -> pure expr

-- A simple Eval function which takes a plain Expr, type checks it
-- and then reduces.
replEvalSimple
  :: ( Binds b (Type tb)
     , Binds tb Kind
     , Abstracts abs tb
     , Eq b
     , Ord tb
     , Document b
     , Document abs
     , Document tb
     )
  => Eval b abs tb (Expr b abs tb)
replEvalSimple expr = do
  ty      <- replTypeCheck expr
  redExpr <- replReduce    expr
  pure $ Just (redExpr,ty)

-- | Feed read text into the Repls configured read function.
replRead
  :: Document o
  => Text
  -> Repl b abs tb o o
replRead input = Repl $ \replState ->
  let readF      = _read . _replConfig $ replState
      grammar    = _someGrammar . _replConfig $ replState
      Repl replF = readF grammar input
    in replF replState

replEval
  :: Eval b abs tb o
replEval a = Repl $ \replState ->
  let evalF = _eval . _replConfig $ replState
      Repl replF = evalF a
   in replF replState

replPrint
  :: Print b abs tb o
replPrint originalTxt a mEvaluated = Repl $ \replState ->
  let printF = _print . _replConfig $ replState
      Repl replF = printF originalTxt a mEvaluated
   in replF replState

-- Read, Eval and return the text to be printed.
-- Drive this function with input, do something with the output and loop for a
-- REPL.
replStep
  :: Document o
  => Text
  -> Repl b abs tb o Doc
replStep input = do
  parsedOutput <- replRead input
  mEvaluated   <- replEval parsedOutput
  replPrint input parsedOutput mEvaluated

