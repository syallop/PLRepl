module PLRepl.TypeCtx.Event
  ( TypeCtxEvent
  , handleTypeCtxEvent
  )
  where

import PLRepl.TypeCtx.State
import PLRepl.Output.Event
import Brick

type TypeCtxEvent = OutputEvent

handleTypeCtxEvent
  :: TypeCtxEvent
  -> TypeCtxState
  -> EventM n TypeCtxState
handleTypeCtxEvent = handleOutputEvent

