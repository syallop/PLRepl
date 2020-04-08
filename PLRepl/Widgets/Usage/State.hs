{-# LANGUAGE OverloadedStrings #-}
module PLRepl.Widgets.Usage.State
  ( UsageState
  , emptyUsageState
  , newUsageState
  , drawUsage
  , usageText
  )
  where

import PLRepl.Widgets.Editor.State

import qualified PLEditor as E

import Brick

import qualified Data.Text as Text

type UsageState = EditorState

emptyUsageState :: UsageState
emptyUsageState = EditorState (E.makeEditor E.emptyLines) (E.tallerView 20 $ E.widerView 1024 $ E.emptyView)

newUsageState
  :: [Text.Text]
  -> UsageState
newUsageState lines = EditorState (E.makeEditor $ foldr E.prependLine E.emptyLines $ map E.textLine lines) (E.tallerView 30 $ E.widerView 1024 $ E.emptyView)

-- Text that overflows the line will be wrapped by Brick. This means:
-- - The containing widget must limit its horizontal length
-- - The editors viewport remains the limit on how long a line will be wrapped
-- before giving up.
-- - TODO: Add wrapping logic to the Editor
drawUsage
  :: n
  -> UsageState
  -> Widget n
drawUsage usageCursor (EditorState editor view) =
  (\(lines,pos) -> Brick.showCursor usageCursor (Location pos) . txtWrap
                                                               . Text.unlines
                                                               . map E.lineText
                                                               . E.renderLines
                                                               $ lines
  ) . E.viewEditor view
    $ editor

usageText
  :: UsageState
  -> Text.Text
usageText = editorText

