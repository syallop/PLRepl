module PLRepl.Widgets.TypeCtx.Event
  ( TypeCtxEvent
  , handleTypeCtxEventDefault
  )
  where

import PLRepl.Widgets.TypeCtx.State
import PLRepl.Widgets.Output.Event


type TypeCtxEvent = OutputEvent

handleTypeCtxEventDefault
  :: TypeCtxEvent
  -> TypeCtxState
  -> TypeCtxState
handleTypeCtxEventDefault = handleOutputEventDefault

