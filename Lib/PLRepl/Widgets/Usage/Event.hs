module PLRepl.Widgets.Usage.Event
  ( UsageEvent
  , handleUsageEventDefault
  )
  where

import PLRepl.Widgets.Usage.State
import PLRepl.Widgets.Output.Event

type UsageEvent = OutputEvent

handleUsageEventDefault
  :: UsageEvent
  -> UsageState
  -> UsageState
handleUsageEventDefault = handleOutputEventDefault

