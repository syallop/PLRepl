module PLRepl.Widgets.Event
  ( Event (..)
  , EditorEvent (..)
  , handleEditorEvent

  , OutputEvent
  , handleOutputEvent

  , TypeCtxEvent
  , handleTypeCtxEvent
  )
  where

import PLRepl.Widgets.State
import PLRepl.Widgets.Editor.Event
import PLRepl.Widgets.Output.Event
import PLRepl.Widgets.TypeCtx.Event

import PLRepl.Repl

import PL.TyVar
import PL.Expr
import PL.Type
import PL.Var

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

  -- The ReplState has been replaced with this value.
  | ReplaceReplState (SomeReplState Var (Type TyVar) TyVar)

