module PLRepl.Editor.Event
  ( EditorEvent (..)
  , handleEditorEvent
  )
  where

import qualified PLEditor as E
import PLRepl.Editor.State
import Brick

-- | An event which may be sent to the Editor widget.
data EditorEvent
  = CursorLeft
  | CursorRight
  | CursorUp
  | CursorDown
  | DeleteChar
  | InsertChar Char
  | NewLine
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

  NewLine
    -> pure $ EditorState (E.newline editor) view

  WiderView i
    -> pure $ EditorState editor (E.widerView i view)

  TallerView i
    -> pure $ EditorState editor (E.tallerView i view)

