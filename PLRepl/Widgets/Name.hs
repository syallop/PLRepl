module PLRepl.Widgets.Name
  ( Name (..)
  , nextFocus
  , previousFocus
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

  | TypeCtxWidget
  | TypeCtxViewport
  | TypeCtxCursor

  | UsageWidget
  | UsageViewport
  | UsageCursor

  -- The entire sidebar.
  | Sidebar
  deriving (Eq, Ord, Show)

-- | Manually map a 'focusable' name to the next.
nextFocus
  :: Name
  -> Name
nextFocus n = case n of
  EditorCursor
    -> OutputCursor

  OutputCursor
    -> TypeCtxCursor

  TypeCtxCursor
    -> UsageCursor

  UsageCursor
    -> EditorCursor

-- | Manually map a 'focusable' name to the previous.
previousFocus
  :: Name
  -> Name
previousFocus n = case n of
  EditorCursor
    -> UsageCursor

  OutputCursor
    -> EditorCursor

  TypeCtxCursor
    -> OutputCursor

  UsageCursor
    -> TypeCtxCursor

