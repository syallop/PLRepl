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

-- | The state an editor begins in.
emptyEditorState :: EditorState
emptyEditorState = EditorState
  { _editor = E.makeEditor E.emptyLines                       -- The initial editors lines are empty
  , _view   = E.tallerView 20 . E.widerView 100 $ E.emptyView -- Give the initial view some size.
  }

-- | Draw editor state as a widget with a cursor 'n'.
drawEditor
  :: n
  -> EditorState
  -> Widget n
drawEditor editorCursor (EditorState editor view) =
  (\(lines,pos) -> Brick.showCursor editorCursor (Location pos) . str
                                                                . Text.unpack
                                                                . Text.unlines
                                                                . map E.lineText
                                                                . E.renderLines
                                                                $ lines
  ) . E.viewEditor view
    $ editor

-- | Get the text contained in an editor by intercalating a newline between each
-- line.
editorText
  :: EditorState
  -> Text.Text
editorText (EditorState editor _view) =
  Text.intercalate "\n" . map E.lineText . E.renderLines . E.editorLines $ editor

