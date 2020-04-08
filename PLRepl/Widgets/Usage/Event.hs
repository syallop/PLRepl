module PLRepl.Widgets.Usage.Event
  ( UsageEvent
  , handleUsageEvent
  )
  where

import PLRepl.Widgets.Usage.State
import PLRepl.Widgets.Output.Event

import Brick

type UsageEvent = OutputEvent

handleUsageEvent
  :: UsageEvent
  -> UsageState
  -> EventM n UsageState
handleUsageEvent = handleOutputEvent

