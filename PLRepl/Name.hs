module PLRepl.Name
  ( Name (..)
  )
  where

-- | Name widgets (and other components).
data Name

  -- The entire repl.
  = ReplApp

  -- The Haskeline widget.
  | Haskeline

  -- The entire sidebar.
  | Sidebar
  deriving (Eq, Ord, Show)
