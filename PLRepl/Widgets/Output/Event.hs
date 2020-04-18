module PLRepl.Widgets.Output.Event
  ( OutputEvent
  , handleOutputEventDefault
  )
  where

import PLRepl.Widgets.Output.State
import PLRepl.Widgets.Editor.Event

type OutputEvent = EditorEvent

handleOutputEventDefault
  :: OutputEvent
  -> OutputState
  -> OutputState
handleOutputEventDefault = handleEditorEventDefault

