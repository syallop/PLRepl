module PLRepl.State
  ( State (..)
  , initialState
  )
  where

import PLRepl.Name

import qualified System.Console.Haskeline.Brick as HL

import PL.Repl
import PL.TyVar
import PL.Var

-- | The state of the entire repl and sub-widgets.
data State = State
  { _replCtx   :: ReplCtx Var TyVar
  , _haskeline :: HL.Widget Name
  }

-- | The initial state of the entire repl and sub-widgets.
initialState :: State
initialState = State
  { _replCtx   = emptyReplCtx
  , _haskeline = HL.initialWidget Haskeline
  }

