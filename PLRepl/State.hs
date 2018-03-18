module PLRepl.State
  ( State (..)
  , EditorState (..)
  , initialState
  , drawEditor
  )
  where

import PLRepl.Name

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
  }

-- | The editor state is the Editor and a View into its lines.
data EditorState = EditorState
  { _editor :: E.Editor
  , _view   :: E.View
  }

-- | The initial state of the entire repl and sub-widgets.
initialState :: State
initialState = State
  { _replCtx     = emptyReplCtx
  , _editorState = EditorState (E.makeEditor E.emptyLines) (E.tallerView 10 $ E.widerView 80 $ E.emptyView)
  }

drawEditor
  :: EditorState
  -> Widget n
drawEditor (EditorState editor view) =
  str . show . Text.unlines . map E.lineText . E.renderLines . E.viewEditor view $ editor

