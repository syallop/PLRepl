module PLRepl.Widgets.Editor.Event
  ( EditorEvent (..)
  , handleEditorEvent
  )
  where

import PLRepl.Widgets.Editor.State
import qualified PLEditor as E

import Brick

import Data.Text (Text)
import qualified Data.Text as Text

-- | An event which may be sent to the Editor widget.
data EditorEvent
  = CursorLeft      -- ^ Move the cursor left
  | CursorRight     -- ^ Move the cursor right
  | CursorUp        -- ^ Move the cursor up
  | CursorDown      -- ^ Move the cursor down
  | DeleteChar      -- ^ Delete a character at the cursor
  | Clear           -- ^ Clear all contents
  | InsertChar Char -- ^ Insert a character at the cursor
  | InsertText Text -- ^ Insert a string of characters at the cursor
  | NewLine         -- ^ Insert a newline
  | WiderView Int   -- ^ Widen (or contract) the width of the viewport
  | TallerView Int  -- ^ Tallen (or shorten) the height of the viewport

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

  Clear
    -> pure $ EditorState (E.makeEditor E.emptyLines) view

  InsertChar c
    -> pure $ EditorState (E.insertCharacter c editor) view

  InsertText txt
    -> pure $ EditorState (Text.foldl (\e c -> if c == '\n' then E.newline e else E.insertCharacter c e) editor txt) view

  NewLine
    -> pure $ EditorState (E.newline editor) view

  WiderView i
    -> pure $ EditorState editor (E.widerView i view)

  TallerView i
    -> pure $ EditorState editor (E.tallerView i view)

