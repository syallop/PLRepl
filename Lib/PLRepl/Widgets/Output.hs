{-|
Module      : PLRepl.Widgets.Output
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

An output text area. This is currently an alias to Editor where we expect
editing events not to be forwarded.
A use case is responses to parsing/ type checking source code entered in an
editor.

|-}
module PLRepl.Widgets.Output
  ( module X
  )
  where

import PLRepl.Widgets.Output.Event as X
import PLRepl.Widgets.Output.State as X

