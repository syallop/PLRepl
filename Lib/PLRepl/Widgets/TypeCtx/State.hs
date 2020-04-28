{-# LANGUAGE OverloadedStrings, GADTs #-}
module PLRepl.Widgets.TypeCtx.State
  ( TypeCtxState
  , emptyTypeCtxState
  , newTypeCtxState
  , typeCtxText

  , ppTypeCtx
  , ppError
  )
  where

import PLRepl.Widgets.Editor.State

import PL.Error
import PL.Kind
import PL.Commented
import PL.Name
import PL.Type
import PL.TypeCtx
import PL.TyVar

import qualified PLEditor as E
import qualified PLPrinter
import PLGrammar
import PLPrinter (Doc, document, pprint)
import PLPrinter.Doc
import PLLispy
import PLLispy.Level

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

type TypeCtxState = EditorState

emptyTypeCtxState :: TypeCtxState
emptyTypeCtxState = EditorState (E.makeEditor E.emptyLines) (E.tallerView 20 $ E.widerView 1024 $ E.emptyView)

newTypeCtxState
  :: [Text.Text]
  -> TypeCtxState
newTypeCtxState lines = EditorState (E.makeEditor $ foldr E.prependLine E.emptyLines $ map E.textLine lines) (E.tallerView 30 $ E.widerView 1024 $ E.emptyView)

typeCtxText
  :: TypeCtxState
  -> Text.Text
typeCtxText = editorText

ppTypeCtx :: Grammar TyVar -> TypeCtx DefaultPhase -> Doc
ppTypeCtx phase = mconcat
             . Map.foldrWithKey
                 (\typeName typeInfo acc -> ppTypeName typeName : lineBreak : indent 2 (ppTypeInfo phase typeInfo) : lineBreak : lineBreak : acc)
                 []
             . typeCtxMapping

ppTypeName :: TypeName -> Doc
ppTypeName (TypeName n) = PLPrinter.char '#' <> PLPrinter.text n

ppTermName :: TermName -> Doc
ppTermName (TermName n) = PLPrinter.char '#' <> PLPrinter.text n

ppTypeInfo :: Grammar TyVar -> TypeInfo DefaultPhase -> Doc
ppTypeInfo tb (TypeInfo isRecursive kind def) = mconcat
    [ ppRec isRecursive
    , lineBreak
    , PLPrinter.text ":: ", ppKind kind
    , lineBreak
    , PLPrinter.text "= ", ppType tb def
    ]

ppType :: Grammar TyVar -> Type -> Doc
ppType tb = fromMaybe mempty . pprint (toPrinter (top $ typ tb)) . addTypeComments

ppRec :: Rec -> Doc
ppRec r = PLPrinter.text $ case r of
  Rec -> "Rec"
  NonRec -> "NonRec"

ppKind :: Kind -> Doc
ppKind k = case k of
  Kind
    -> PLPrinter.text "KIND"
  KindArrow from to
    -> PLPrinter.char '^' <> parens (ppKind from) <> parens (ppKind to)

