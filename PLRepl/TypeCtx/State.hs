module PLRepl.TypeCtx.State
  ( TypeCtxState
  , emptyTypeCtxState
  , newTypeCtxState
  , drawTypeCtx
  , typeCtxText
  )
  where

import Brick
import qualified PLEditor as E
import qualified Data.Text as Text
import PLRepl.Editor.State

type TypeCtxState = EditorState

emptyTypeCtxState :: TypeCtxState
emptyTypeCtxState = EditorState (E.makeEditor E.emptyLines) (E.tallerView 20 $ E.widerView 80 $ E.emptyView)

newTypeCtxState
  :: [Text.Text]
  -> TypeCtxState
newTypeCtxState lines = EditorState (E.makeEditor $ foldr E.prependLine E.emptyLines $ map E.textLine lines) (E.tallerView 30 $ E.widerView 80 $ E.emptyView)

drawTypeCtx
  :: n
  -> TypeCtxState
  -> Widget n
drawTypeCtx = drawEditor

typeCtxText
  :: TypeCtxState
  -> Text.Text
typeCtxText = editorText


