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
emptyOutputState = EditorState (E.makeEditor E.emptyLines) (E.tallerView 20 $ E.widerView 80 $ E.emptyView)

newOutputState
  :: [Text.Text]
  -> OutputState
newOutputState lines = EditorState (E.makeEditor $ foldr E.prependLine E.emptyLines $ map E.textLine lines) (E.tallerView 30 $ E.widerView 80 $ E.emptyView)

drawOutput
  :: n
  -> OutputState
  -> Widget n
drawOutput = drawEditor

outputText
  :: OutputState
  -> Text.Text
outputText = editorText

