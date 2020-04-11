{-# LANGUAGE OverloadedStrings #-}
module PLRepl.Widgets.TypeCtx.State
  ( TypeCtxState
  , emptyTypeCtxState
  , newTypeCtxState
  , drawTypeCtx
  , typeCtxText

  , ppTypeCtx
  , ppError
  )
  where

import PLRepl.Widgets.Editor.State

import PL.Error
import PL.Kind
import PL.Name
import PL.Type
import PL.TypeCtx

import qualified PLEditor as E
import qualified PLPrinter
import PLGrammar
import PLPrinter (Doc, document, pprint)
import PLPrinter.Doc
import PLLispy

import Brick

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

ppTypeCtx :: (Show tb, Ord tb) => Grammar tb -> TypeCtx tb -> Doc
ppTypeCtx tb = mconcat
             . Map.foldrWithKey
                 (\typeName typeInfo acc -> ppTypeName typeName : lineBreak : indent 2 (ppTypeInfo tb typeInfo) : lineBreak : lineBreak : acc)
                 []
             . typeCtxMapping

ppTypeName :: TypeName -> Doc
ppTypeName (TypeName n) = PLPrinter.char '#' <> PLPrinter.text n

ppTermName :: TermName -> Doc
ppTermName (TermName n) = PLPrinter.char '#' <> PLPrinter.text n

ppTypeInfo :: (Show tb, Ord tb) => Grammar tb -> TypeInfo tb -> Doc
ppTypeInfo tb (TypeInfo isRecursive kind def) = mconcat
    [ ppRec isRecursive
    , lineBreak
    , PLPrinter.text ":: ", ppKind kind
    , lineBreak
    , PLPrinter.text "= ", ppType tb def
    ]

ppType :: (Show tb, Ord tb) => Grammar tb -> Type tb -> Doc
ppType tb t = fromMaybe mempty $ pprint (toPrinter (typ tb)) t

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

ppError :: (Show tb, Ord tb) => Grammar tb -> Error tb -> Doc
ppError tb e = case e of
  EMsg doc
    -> doc

  ETypeNotDefined name
    -> mconcat [ PLPrinter.text "Type named '"
               , ppTypeName name
               , PLPrinter.text "' is not defined."
               ]

  ETermNotDefined name
    -> mconcat [ PLPrinter.text "Term named '"
               , ppTermName name
               , PLPrinter.text "' is not defined."
               ]

  EAppMismatch fTy xTy
    -> mconcat [ PLPrinter.text "Cannot apply expression typed: '"
               , ppType tb fTy
               , PLPrinter.text "' to expression typed: '"
               , ppType tb xTy
               , PLPrinter.text "'."
               ]

  EBigAppMismatch fTy xKy
    -> mconcat [ PLPrinter.text "Cannot big-apply expression typed: '"
               , ppType tb fTy
               , PLPrinter.text "' to type kinded: '"
               , ppKind xKy
               , PLPrinter.text "'."
               ]

  ETypeAppMismatch fKy xKy
    -> mconcat [ PLPrinter.text "Cannot type-apply type kinded: '"
               , ppKind fKy
               , PLPrinter.text "' to type kinded: '"
               , ppKind xKy
               , PLPrinter.text "'."
               ]

  ETypeAppLambda fTy
    -> mconcat [ PLPrinter.text "Cannot type-apply a non type-lam: "
               , ppType tb fTy
               ]

  ESumMismatch actualType index sumTys
    -> mconcat [ PLPrinter.text "Expression had type: "
               , ppType tb actualType
               , PLPrinter.text "and claimed to be contained within the sum"
               , mconcat . NE.toList . fmap (ppType tb) $ sumTys
               , PLPrinter.text "at index"
               , document index
               ]

  ECaseDefaultMismatch defaultTy firstBranchTy
    -> mconcat [ PLPrinter.text "In a case statement the default branch had type: "
               , ppType tb defaultTy
               , PLPrinter.text "whereas the first branch had type: "
               , ppType tb firstBranchTy
               , PLPrinter.text " but branches must have the same type."
               ]

