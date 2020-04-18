{-# LANGUAGE OverloadedStrings #-}
module PLRepl.Widgets.Editor.State
  ( EditorState (..)
  , emptyEditorState
  , editorText
  )
  where

import qualified PLEditor as E

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
  , _view   = E.tallerView 20 . E.widerView 160 $ E.emptyView -- Give the initial view some size.
  }

-- | Get the text contained in an editor by intercalating a newline between each
-- line.
editorText
  :: EditorState
  -> Text.Text
editorText (EditorState editor _view) =
  Text.intercalate "\n" . map E.lineText . E.renderLines . E.editorLines $ editor

