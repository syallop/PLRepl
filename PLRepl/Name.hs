module PLRepl.Name
  ( Name (..)
  )
  where

-- | Name widgets (and other components).
data Name

  -- The entire repl.
  = ReplApp

  -- The editor widget.
  | EditorWidget
  | EditorViewport
  | EditorCursor

  | OutputWidget
  | OutputViewport
  | OutputCursor

  -- The entire sidebar.
  | Sidebar
  deriving (Eq, Ord, Show)
