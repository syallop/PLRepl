module PLRepl.Event
  ( Event (..)
  , EditorEvent (..)
  , handleEditorEvent

  , OutputEvent
  , handleOutputEvent

  , TypeCtxEvent
  , handleTypeCtxEvent
  )
  where

import PL.Repl
import PL.TyVar
import PL.Var
import PLRepl.State
import PLRepl.Editor.Event
import PLRepl.Output.Event
import PLRepl.TypeCtx.Event

-- | Events the repl and its sub-components may emit and handle.
data Event n

  -- An event to the Editor.
  = EditorEv EditorEvent

  -- An event to the Output.
  | OutputEv OutputEvent

  -- An event to the TypeCtx output.
  | TypeCtxEv TypeCtxEvent

  -- Toggle focus to a name.
  | FocusOn (Maybe n)

  -- The ReplCtx has been replaced with this value.
  | ReplaceReplCtx (ReplCtx Var TyVar)

