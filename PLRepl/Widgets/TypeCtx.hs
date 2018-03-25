module PLRepl.Widgets.TypeCtx
  ( module X
  , drawTypeCtx
  )
  where

import PLRepl.Widgets.TypeCtx.Event as X
import PLRepl.Widgets.TypeCtx.State as X

import PL

drawTypeCtx
  :: TypeCtx tb
  -> Widget PL.Name
drawTypeCtx typeCtx =
  str . Text.unpack . Text.unlines . document $ typeCtx

