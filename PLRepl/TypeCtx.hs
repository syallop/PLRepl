module PLRepl.TypeCtx
  ( module X
  , drawTypeCtx
  )
  where

import PLRepl.TypeCtx.Event as X
import PLRepl.TypeCtx.State as X

import PL

drawTypeCtx
  :: TypeCtx tb
  -> Widget PL.Name
drawTypeCtx typeCtx =
  str . Text.unpack . Text.unlines . document $ typeCtx

