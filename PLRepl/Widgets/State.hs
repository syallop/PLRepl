{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , RankNTypes
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

import PL.Grammar.Lispy

import PLPrinter
import qualified PLEditor as E

import Brick
import Data.Maybe
import qualified Data.Text as Text

-- | The state of the entire repl and sub-widgets.
data State n = State
  { -- The state of the Repl includes types, expressions and bindings etc.
    -- Currently configured with a Grammar on Expr Var (Type TyVar) TyVar
    _replState    :: SomeReplState Var (Type TyVar) TyVar

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
initialState :: Maybe n -> State n
initialState initialFocus = State
  { _replState    = initialReplState
  , _editorState  = emptyEditorState
  , _outputState  = emptyOutputState
  , _typeCtxState = initialTypeCtxState
  , _focusOn      = initialFocus
  }
  where
    initialReplState :: SomeReplState Var (Type TyVar) TyVar
    initialReplState =
      let ReplState _ exprBindCtx typeBindCtx typeBindings typeCtx = emptyReplState
          replConfig = lispyExprReplConfig megaparsecGrammarParser var (typ tyVar) tyVar
       in SomeReplState $ ReplState replConfig exprBindCtx typeBindCtx typeBindings
            $ fromJust $ insertType "Bool" (fixType $ SumT $ map fixType $ [ProductT [], ProductT []])
            $ fromJust $ insertType "Unit" (fixType $ SumT []) typeCtx

    initialTypeCtxState = typeCtxStateGivenReplState initialReplState

-- | What is the typeCtxState output given the current ReplState.
typeCtxStateGivenReplState
  :: SomeReplState Var (Type TyVar) TyVar
  -> TypeCtxState
typeCtxStateGivenReplState (SomeReplState replState) =
  newTypeCtxState . Text.lines . renderDocument . _typeCtx $ replState

