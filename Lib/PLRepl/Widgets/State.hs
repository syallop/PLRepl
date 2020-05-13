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
  , typeCtxStateGivenReplState
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
import PL.Var
import PL.Test.Shared

import PLLispy
import PLLispy.Level

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

-- | A Name given to a Grammar.
-- TODO:
-- - Maybe Grammars themselves should be named/ names should be moved to
--   PLGrammar.
type GrammarName = Text

-- | The state of the entire repl and sub-widgets.
data State n = State
  { -- The state of the Repl includes types, expressions and bindings etc.
    -- Currently configured with a Grammar on Expr Var Type TyVar
    _replState    :: SomeReplState

    -- A Map of grammar names to repl configs with the intent we can swap out
    -- grammars at runtime in order to parse, eval and print things differently.
  , _replConfigs  :: Map GrammarName SomeReplConfig

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
  -> HashStore CommentedExpr
  -> State n
initialState initialFocus usage backingStorage = State
  { _replState    = initialReplState
  , _replConfigs  = initialReplConfigs
  , _editorState  = emptyEditorState
  , _outputState  = emptyOutputState
  , _typeCtxState = initialTypeCtxState
  , _usageState   = newUsageState usage
  , _focusOn      = initialFocus
  }
  where
    -- Take the emptyReplState, then add an initial non-empty replconfig and a
    -- few example types.
    initialReplState :: SomeReplState
    initialReplState =
      let ReplState _ exprBindCtx typeBindCtx typeBindings typeCtx _exprStore = (emptyReplState :: ReplState ())
       in SomeReplState $ ReplState
            { _replConfig   = exprConfig
            , _exprBindCtx  = exprBindCtx
            , _typeBindCtx  = typeBindCtx
            , _typeBindings = typeBindings
            , _typeCtx      = sharedTypeCtx
            , _exprStore    = backingStorage
            }

    initialTypeCtxState = typeCtxStateGivenReplState initialReplState

    exprGrammar :: G.Grammar CommentedExpr
    exprGrammar = top $ expr var (sub $ typ tyVar) tyVar

    exprConfig :: ReplConfig CommentedExpr
    exprConfig = lispyExprReplConfig (plGrammarParser exprGrammar) var (sub $ typ tyVar) tyVar

    typePrinter :: CommentedType -> Doc
    typePrinter = fromMaybe mempty . pprint (toPrinter $ top $ typ tyVar)

    typeGrammar :: G.Grammar CommentedType
    typeGrammar = top $ typ tyVar

    typeConfig :: ReplConfig CommentedType
    typeConfig = lispyTypeReplConfig (plGrammarParser typeGrammar) tyVar

      where
    initialReplConfigs :: Map GrammarName SomeReplConfig
    initialReplConfigs = Map.fromList
      [ ("lispyExpr", SomeReplConfig exprConfig)
      , ("lispyType", SomeReplConfig typeConfig)
      ]

-- | What is the typeCtxState output given the current ReplState.
typeCtxStateGivenReplState
  :: SomeReplState
  -> TypeCtxState
typeCtxStateGivenReplState (SomeReplState replState)
  = newTypeCtxState . Text.lines . (PLPrinter.render . ppTypeCtx document (ppTypeInfo ppType)) . _typeCtx $ replState
  where
    ppType = fromMaybe mempty . pprint (toPrinter $ top $ typ tyVar) . addTypeComments

instance Document a => Document [a] where
  document []     = mempty
  document (a:as) = document a <> document as

instance Document a => Document (NE.NonEmpty a) where
  document as = document . NE.toList $ as
