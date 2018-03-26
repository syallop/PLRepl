{-# LANGUAGE OverloadedStrings #-}
module PLRepl.Widgets.Editor.State
  ( EditorState (..)
  , emptyEditorState
  , drawEditor
  , editorText
  )
  where

import qualified PLEditor as E

import Brick
import qualified Data.Text as Text

-- | The editor state is the Editor and a View into its lines.
data EditorState = EditorState
  { _editor :: E.Editor
  , _view   :: E.View
  }

emptyEditorState :: EditorState
emptyEditorState = EditorState (E.makeEditor E.emptyLines) (E.tallerView 20 $ E.widerView 80 $ E.emptyView)

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
  Text.intercalate "\n" . map E.lineText . E.renderLines . E.editorLines $ editor

