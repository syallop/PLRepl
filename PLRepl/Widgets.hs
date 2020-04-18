{-# LANGUAGE
    RankNTypes
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , UndecidableInstances
  #-}
{-|
Module      : PLRepl.Widgets
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Re-export composite state and events for several widgets including
editor, output, typectx and usage.
|-}
module PLRepl.Widgets
  ( module X
  )
  where

import PLRepl.Widgets.Editor  as X
import PLRepl.Widgets.Output  as X
import PLRepl.Widgets.TypeCtx as X
import PLRepl.Widgets.Usage   as X

import PLRepl.Widgets.Event as X
import PLRepl.Widgets.State as X
