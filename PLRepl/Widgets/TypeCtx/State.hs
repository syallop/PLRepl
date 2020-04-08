module PLRepl.Widgets.TypeCtx.State
  ( TypeCtxState
  , emptyTypeCtxState
  , newTypeCtxState
  , drawTypeCtx
  , typeCtxText
  )
  where

import PLRepl.Widgets.Editor.State

import qualified PLEditor as E

import Brick

import qualified Data.Text as Text

type TypeCtxState = EditorState

emptyTypeCtxState :: TypeCtxState
emptyTypeCtxState = EditorState (E.makeEditor E.emptyLines) (E.tallerView 20 $ E.widerView 1024 $ E.emptyView)

newTypeCtxState
  :: [Text.Text]
  -> TypeCtxState
newTypeCtxState lines = EditorState (E.makeEditor $ foldr E.prependLine E.emptyLines $ map E.textLine lines) (E.tallerView 30 $ E.widerView 1024 $ E.emptyView)

drawTypeCtx
  :: n
  -> TypeCtxState
  -> Widget n
drawTypeCtx typeCtxCursor (EditorState editor view) =
  (\(lines,pos) -> Brick.showCursor typeCtxCursor (Location pos) . txtWrap
                                                                 . Text.unlines
                                                                 . map E.lineText
                                                                 . E.renderLines
                                                                 $ lines
  ) . E.viewEditor view
    $ editor

typeCtxText
  :: TypeCtxState
  -> Text.Text
typeCtxText = editorText

