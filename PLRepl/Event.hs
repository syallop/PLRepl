module PLRepl.Event
  ( Event (..)
  )
  where

import qualified System.Console.Haskeline.Brick as HL

import Control.Exception

import PL.Repl
import PL.TyVar
import PL.Var

-- | Events the repl and its sub-components may emit and handle.
data Event

  -- An event emitted from haskeline.
  = HaskelineEv HL.ToBrick

  -- Haskeline died, possibly as the result of throwing an exception.
  | HaskelineDied (Either SomeException ())

  -- The ReplCtx has been replaced with this value.
  | ReplaceReplCtx (ReplCtx Var TyVar)

