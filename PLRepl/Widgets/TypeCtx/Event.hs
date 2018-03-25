module PLRepl.Widgets.TypeCtx.Event
  ( TypeCtxEvent
  , handleTypeCtxEvent
  )
  where

import PLRepl.Widgets.TypeCtx.State
import PLRepl.Widgets.Output.Event

import Brick

type TypeCtxEvent = OutputEvent

handleTypeCtxEvent
  :: TypeCtxEvent
  -> TypeCtxState
  -> EventM n TypeCtxState
handleTypeCtxEvent = handleOutputEvent

