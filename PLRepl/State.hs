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

-- | The editor state is the Editor and a View into its lines.
data EditorState = EditorState
  { _editor :: E.Editor
  , _view   :: E.View
  }

type OutputState = EditorState

-- | The initial state of the entire repl and sub-widgets.
initialState :: State
initialState = State
  { _replCtx     = emptyReplCtx
  , _editorState = emptyEditorState
  , _outputState = emptyOutputState
  , _focusEditor = True
  }

emptyEditorState :: EditorState
emptyEditorState = EditorState (E.makeEditor E.emptyLines) (E.tallerView 20 $ E.widerView 80 $ E.emptyView)

emptyOutputState :: OutputState
emptyOutputState = EditorState (E.makeEditor E.emptyLines) (E.tallerView 20 $ E.widerView 80 $ E.emptyView)

newOutputState
  :: [Text.Text]
  -> OutputState
newOutputState lines = EditorState (E.makeEditor $ foldr E.prependLine E.emptyLines $ map E.textLine lines) (E.tallerView 30 $ E.widerView 80 $ E.emptyView)

drawEditor
  :: n
  -> EditorState
  -> Widget n
drawEditor editorCursor (EditorState editor view) =
  (\(lines,pos) -> Brick.showCursor editorCursor (Location pos) . str . Text.unpack . Text.unlines . map E.lineText . E.renderLines $ lines) . E.viewEditor view $ editor

editorText
  :: EditorState
  -> Text.Text
editorText (EditorState editor _view) =
  Text.unlines . map E.lineText . E.renderLines . E.editorLines $ editor

drawOutput
  :: n
  -> OutputState
  -> Widget n
drawOutput = drawEditor

outputText
  :: OutputState
  -> Text.Text
outputText = editorText


