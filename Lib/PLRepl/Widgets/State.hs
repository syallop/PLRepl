{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , RankNTypes
  , ImplicitParams
  #-}
module PLRepl.Widgets.State
  ( State (..)
  , EditorState (..)
  , initialState
  , editorText
  , emptyEditorState

  , OutputState
  , outputText
  , emptyOutputState
  , newOutputState

  , TypeCtxState
  , typeCtxText
  , emptyTypeCtxState
  , newTypeCtxState
  , typeCtxStateGivenReplTypeCtx
  , ppTypeCtx
  , ppError

  , UsageState
  , usageText
  , emptyUsageState
  , newUsageState
  )
  where

import PLRepl.Widgets.Editor.State
import PLRepl.Widgets.Name
import PLRepl.Widgets.Output.State
import PLRepl.Widgets.TypeCtx.State
import PLRepl.Widgets.Usage.State

import PLRepl.Repl
import PLRepl.Repl.Lispy

import PL.Expr
import PL.Commented
import PL.TyVar
import PL.Type
import PL.Store
import PL.Hash
import PL.HashStore
import PL.TypeCtx
import PL.CodeStore
import PL.FixPhase
import PL.Var
import PL.TypeCheck
import PL.Test.Shared

import PLLispy
import PLLispy.Name
import PLLispy.Level

import Reversible

import qualified PLGrammar as G

import PLPrinter
import qualified PLEditor as E

import Data.Maybe
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.List.NonEmpty as NE

import Data.Text (Text)
import Data.Char

import qualified Data.List.NonEmpty as NE
import Data.List

import Data.Monoid

-- | The state of the entire repl and sub-widgets.
data State n = State
  {
    -- The current repl can be 'stepped' with textual input in order to read,
    -- evaluate and print some output as defined by its configuration.
    _currentRepl  :: SimpleRepl

   -- The editorState corresponds to an input widget into which expressions are
   -- entered.
  , _editorState  :: EditorState

  -- The outputState corresponds to an output widget used to display failures/
  -- successes as a result of attempting to enter expressions using the
  -- editorState. Typically this will be parse errors or type errors.
  , _outputState  :: OutputState

  -- The typeCtxState corresponds to an output widget used to display the
  -- current TypeCtx which is held in the ReplCtx.
  , _typeCtxState :: TypeCtxState

  -- The usageState corresponds to an output widget used to display usage
  -- information.
  , _usageState   :: UsageState

  -- A possible named widget to focus input on.
  , _focusOn      :: Maybe n
  }

-- | The initial state of the entire repl and sub-widgets.
--
-- This function is a particular mess and is currently being used to drive
-- testing by switching out the repl config and trialing input.
initialState
  :: Maybe n
  -> [Text]
  -> CodeStore
  -> State n
initialState initialFocus usage codeStore = State
  { _currentRepl  = currentRepl
  , _editorState  = emptyEditorState
  , _outputState  = emptyOutputState
  , _typeCtxState = initialTypeCtxState
  , _usageState   = newUsageState usage
  , _focusOn      = initialFocus
  }
  where
    currentRepl :: SimpleRepl
    currentRepl = lispyExprRepl codeStore

    initialTypeCtxState = typeCtxStateGivenReplTypeCtx . _replTypeCtx . simpleReplCtx $ currentRepl

-- | What is the typeCtxState output given the source of truth TypeCtx in the
-- Repl.
typeCtxStateGivenReplTypeCtx
  :: TypeCtx
  -> TypeCtxState
typeCtxStateGivenReplTypeCtx typeCtx
  = newTypeCtxState . Text.lines . (PLPrinter.render . ppTypeCtx document (ppTypeInfo ppType)) $ typeCtx
  where
    ppType = fromMaybe mempty . pprint (toPrinter $ top $ typ typeDeps)

    typeDeps :: TypeGrammarDependencies DefaultPhase
    typeDeps = TypeGrammarDependencies
      { _typeBindingFor        = tyVar
      , _typeContentBindingFor = contentNameGrammar

      , _namedGrammarExtension              = noExtG
      , _arrowGrammarExtension              = noExtG
      , _sumTGrammarExtension               = noExtG
      , _productTGrammarExtension           = noExtG
      , _unionTGrammarExtension             = noExtG
      , _bigArrowGrammarExtension           = noExtG
      , _typeLamGrammarExtension            = noExtG
      , _typeAppGrammarExtension            = noExtG
      , _typeBindingGrammarExtension        = noExtG
      , _typeContentBindingGrammarExtension = noExtG

      , _typeGrammarExtension = noExtG
      }

    noExtG :: G.Grammar NoExt
    noExtG = rpure noExt

instance Document a => Document [a] where
  document []     = mempty
  document (a:as) = document a <> document as

instance Document a => Document (NE.NonEmpty a) where
  document as = document . NE.toList $ as

