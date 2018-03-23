module PLRepl.Output.Event
  ( OutputEvent
  , handleOutputEvent
  )
  where

import PLRepl.Output.State
import PLRepl.Editor.Event
import Brick

type OutputEvent = EditorEvent

handleOutputEvent
  :: OutputEvent
  -> OutputState
  -> EventM n OutputState
handleOutputEvent = handleEditorEvent

