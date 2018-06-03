module PLRepl.Widgets.Output.State
  ( OutputState
  , emptyOutputState
  , newOutputState
  , drawOutput
  , outputText
  )
  where
import PLRepl.Widgets.Editor.State

import qualified PLEditor as E

import Brick
import qualified Data.Text as Text

type OutputState = EditorState

emptyOutputState :: OutputState
emptyOutputState = EditorState
  { _editor = E.makeEditor E.emptyLines
  , _view   = E.tallerView 20 $ E.widerView 80 $ E.emptyView
  }

newOutputState
  :: [Text.Text]
  -> OutputState
newOutputState lines = EditorState
  { _editor = E.makeEditor $ foldr E.prependLine E.emptyLines $ map E.textLine lines
  , _view   = E.tallerView 20 $ E.widerView 80 $ E.emptyView
  }

drawOutput
  :: n
  -> OutputState
  -> Widget n
drawOutput = drawEditor

outputText
  :: OutputState
  -> Text.Text
outputText = editorText

