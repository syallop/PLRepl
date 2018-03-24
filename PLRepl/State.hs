{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
module PLRepl.State
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

import PLRepl.Name
import PLRepl.Editor.State
import PLRepl.Output.State
import PLRepl.TypeCtx.State

import qualified PLEditor as E
import PL.Repl
import PL.TyVar
import PL.Var
import PL.Type
import PL.TypeCtx
import PL.FixType

import PLPrinter

import Brick
import qualified Data.Text as Text
import Data.Maybe

-- | The state of the entire repl and sub-widgets.
data State n = State
  { -- The state of the Repl includes types, expressions and bindings etc.
    _replCtx      :: ReplCtx Var TyVar

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
  { _replCtx      = initialReplCtx
  , _editorState  = emptyEditorState
  , _outputState  = emptyOutputState
  , _typeCtxState = initialTypeCtxState
  , _focusOn      = initialFocus
  }
  where
    initialReplCtx =
      let ReplCtx exprBindCtx typeBindCtx typeBindings typeCtx = emptyReplCtx
       in ReplCtx exprBindCtx typeBindCtx typeBindings $ fromJust $ insertType "Bool" (fixType $ SumT $ map fixType $ [ProductT [], ProductT []])
                                                       $ fromJust $ insertType "Unit" (fixType $ SumT []) typeCtx

    initialTypeCtxState = typeCtxStateGivenReplCtx initialReplCtx

-- | What is the typeCtxState output given the current ReplCtx.
typeCtxStateGivenReplCtx
  :: ReplCtx Var TyVar
  -> TypeCtxState
typeCtxStateGivenReplCtx =
  newTypeCtxState . Text.lines . renderDocument . _typeCtx

