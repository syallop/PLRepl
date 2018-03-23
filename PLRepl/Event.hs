module PLRepl.Event
  ( Event (..)
  , EditorEvent (..)
  , handleEditorEvent

  , OutputEvent
  , handleOutputEvent
  )
  where

import PL.Repl
import PL.TyVar
import PL.Var
import PLRepl.State
import PLRepl.Editor.Event
import PLRepl.Output.Event

-- | Events the repl and its sub-components may emit and handle.
data Event

  -- An event to the Editor.
  = EditorEv EditorEvent

  -- An event to the Output.
  | OutputEv OutputEvent

  -- Toggle focus between Editor and Output.
  | ToggleFocus

  -- The ReplCtx has been replaced with this value.
  | ReplaceReplCtx (ReplCtx Var TyVar)

