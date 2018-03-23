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
  )
  where

import PLRepl.Name
import PLRepl.Editor.State
import PLRepl.Output.State

import qualified PLEditor as E
import PL.Repl
import PL.TyVar
import PL.Var

import Brick
import qualified Data.Text as Text

-- | The state of the entire repl and sub-widgets.
data State = State
  { _replCtx     :: ReplCtx Var TyVar
  , _editorState :: EditorState
  , _outputState :: OutputState
  , _focusEditor :: Bool
  }

-- | The initial state of the entire repl and sub-widgets.
initialState :: State
initialState = State
  { _replCtx     = emptyReplCtx
  , _editorState = emptyEditorState
  , _outputState = emptyOutputState
  , _focusEditor = True
  }

