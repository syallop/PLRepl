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
  , drawEditor
  , editorText
  , emptyEditorState

  , OutputState
  , drawOutput
  , outputText
  , emptyOutputState
  , newOutputState

  , TypeCtxState
  , drawTypeCtx
  , typeCtxText
  , emptyTypeCtxState
  , newTypeCtxState
  )
  where

import PLRepl.Widgets.Editor.State
import PLRepl.Widgets.Name
import PLRepl.Widgets.Output.State
import PLRepl.Widgets.TypeCtx.State

import PLRepl.Repl
import PLRepl.Repl.Lispy

import PL.Expr
import PL.FixType
import PL.TyVar
import PL.Type
import PL.TypeCtx
import PL.Var

import PLLispy

import qualified PLGrammar as G

import PLPrinter
import qualified PLEditor as E

import Brick
import Data.Maybe
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

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
    -- Currently configured with a Grammar on Expr Var (Type TyVar) TyVar
    _replState    :: SomeReplState Var (Type TyVar) TyVar

    -- A Map of grammar names to repl configs with the intent we can swap out
    -- grammars at runtime in order to parse, eval and print things differently.
  , _replConfigs  :: Map GrammarName (SomeReplConfig Var (Type TyVar) TyVar)

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

  -- A possible named widget to focus input on.
  , _focusOn      :: Maybe n
  }

-- | The initial state of the entire repl and sub-widgets.
--
-- This function is a particular mess and is currently being used to drive
-- testing by switching out the repl config and trialing input.
initialState :: Maybe n -> State n
initialState initialFocus = State
  { _replState    = initialReplState
  , _replConfigs  = initialReplConfigs
  , _editorState  = emptyEditorState
  , _outputState  = emptyOutputState
  , _typeCtxState = initialTypeCtxState
  , _focusOn      = initialFocus
  }
  where
    -- Take the emptyReplState, then add an initial non-empty replconfig and a
    -- few example types.
    initialReplState :: SomeReplState Var (Type TyVar) TyVar
    initialReplState =
      let ReplState _ exprBindCtx typeBindCtx typeBindings typeCtx = emptyReplState
       in SomeReplState $ ReplState (exprConfig)
                                    exprBindCtx
                                    typeBindCtx
                                    typeBindings
            $ fromJust $ insertType "Bool" (fixType $ SumT $ map fixType $ [ProductT [], ProductT []])
            $ fromJust $ insertRecType "Nat" (fixType $ SumT $ map fixType $ [ProductT [], Named "Nat"])
            $ fromJust $ insertType "Unit" (fixType $ SumT []) typeCtx

    initialTypeCtxState = typeCtxStateGivenReplState initialReplState

    exprConfig = lispyExprReplConfig plGrammarParser var (typ tyVar) tyVar
    typeConfig = lispyTypeReplConfig plGrammarParser tyVar

      where
    initialReplConfigs :: Map GrammarName (SomeReplConfig Var (Type TyVar) TyVar)
    initialReplConfigs = Map.fromList
      [ ("lispyExpr", SomeReplConfig exprConfig)
      , ("lispyType", SomeReplConfig typeConfig)
      ]

-- | What is the typeCtxState output given the current ReplState.
typeCtxStateGivenReplState
  :: SomeReplState Var (Type TyVar) TyVar
  -> TypeCtxState
typeCtxStateGivenReplState (SomeReplState replState) =
  newTypeCtxState . Text.lines . renderDocument . _typeCtx $ replState

instance Document a => Document [a] where
  document []     = mempty
  document (a:as) = document a <> document as

instance Document a => Document (NE.NonEmpty a) where
  document as = document . NE.toList $ as
