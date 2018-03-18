module PLRepl.Event
  ( Event (..)
  , EditorEvent (..)
  , handleEditorEvent
  )
  where

import PL.Repl
import PL.TyVar
import PL.Var
import qualified PLEditor as E
import PLRepl.State

import Brick

-- | Events the repl and its sub-components may emit and handle.
data Event

  -- An event emitted from the editor.
  = EditorEv EditorEvent

  -- The ReplCtx has been replaced with this value.
  | ReplaceReplCtx (ReplCtx Var TyVar)

data EditorEvent
  = CursorLeft
  | CursorRight
  | CursorUp
  | CursorDown
  | DeleteChar
  | InsertChar Char
  | WiderView Int
  | TallerView Int

-- | Handle editor specific events.
handleEditorEvent
  :: EditorEvent
  -> EditorState
  -> EventM n EditorState
handleEditorEvent editorEv (EditorState editor view) = case editorEv of
  CursorLeft
    -> pure $ EditorState (E.tryMoveLeft editor) view

  CursorRight
    -> pure $ EditorState (E.tryMoveRight editor) view

  CursorUp
    -> pure $ EditorState (E.tryMoveUp editor) view

  CursorDown
    -> pure $ EditorState (E.tryMoveDown editor) view

  DeleteChar
    -> let (nextEditor,mChar) = E.deleteCharacter editor
        in pure $ EditorState nextEditor view

  InsertChar c
    -> pure $ EditorState (E.insertCharacter c editor) view

  WiderView i
    -> pure $ EditorState editor (E.widerView i view)

  TallerView i
    -> pure $ EditorState editor (E.tallerView i view)

