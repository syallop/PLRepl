{-# LANGUAGE OverloadedStrings #-}
module PLRepl.Widgets.Usage.State
  ( UsageState
  , emptyUsageState
  , newUsageState
  , usageText
  )
  where

import PLRepl.Widgets.Editor.State

import qualified PLEditor as E

import qualified Data.Text as Text

type UsageState = EditorState

emptyUsageState :: UsageState
emptyUsageState = EditorState (E.makeEditor E.emptyLines) (E.tallerView 20 $ E.widerView 1024 $ E.emptyView)

newUsageState
  :: [Text.Text]
  -> UsageState
newUsageState lines = EditorState (E.makeEditor $ foldr E.prependLine E.emptyLines $ map E.textLine lines) (E.tallerView 30 $ E.widerView 1024 $ E.emptyView)

usageText
  :: UsageState
  -> Text.Text
usageText = editorText

