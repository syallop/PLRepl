module PLRepl.Widgets.Output.Event
  ( OutputEvent
  , handleOutputEvent
  )
  where

import PLRepl.Widgets.Output.State
import PLRepl.Widgets.Editor.Event
import Brick

type OutputEvent = EditorEvent

handleOutputEvent
  :: OutputEvent
  -> OutputState
  -> EventM n OutputState
handleOutputEvent = handleEditorEvent

