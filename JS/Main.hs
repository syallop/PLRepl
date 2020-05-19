{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

-- | Our modules
import PLReplJS.LocalStorage

-- | Miso framework import
import Miso
import Miso.String

import GHCJS.Prim
import GHCJS.Types
import GHCJS.Marshal

-- PL dependencies
import PL.Error
import PL.Pattern
import PL.Expr hiding (Expr, App)
import PL.Commented
import PL.Kind
import PL.TyVar
import PL.Type hiding (Type)
import PL.TypeCtx
import PL.Var
import PL.TypeCheck
import PL.CodeStore
import PL.Store
import PL.Store.Nested
import PL.Store.File
import PL.Store.Memory
import PL.Hash
import PL.HashStore
import PL.Serialize

import PLGrammar
import PLLispy
import PLLispy.Level
import PLPrinter (lineBreak,indent,pprint,document)
import PLPrinter.Doc (parens)
import PLRepl.Repl
import PLRepl.Repl.Lispy
import PLRepl.Widgets.Event hiding (Event)
import PLRepl.Widgets.Name

import qualified PL as PL
import qualified PL.Expr as PL
import qualified PL.Name as PL
import qualified PL.Test.Expr as Test
import qualified PL.Test.ExprTestCase as Test
import qualified PL.Type as PL
import qualified PLEditor as E
import qualified PLLispy.Test.Sources.Expr as Test
import qualified PLPrinter
import qualified PLRepl.Widgets.Event as PL
import qualified PLRepl.Widgets.State as PL

import qualified PLParser as PLParser
import qualified PLPrinter as PLPrinter
import qualified PLGrammar as G
import qualified PLLispy as L
import qualified PLLispy.Level as L

-- Other dependencies
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import System.Random
import qualified Data.Map as Map
import qualified Data.Text as Text

-- Aliases for the concrete expression/ types we're going to use
type Expr = PL.Expr
type Type = PL.Type

newtype State n = State (PL.State n)

-- TODO: Presumably is we defined some partial equality here we could avoid
-- doing computations on identical states?
instance Eq (State n) where
 _ == _ = False

data Event n
  = NoOp

  | PLEvent (PL.Event n)

  | ReplaceEditorText Text

  | RandomExpression

  | Read
  | Eval Text
  | PrintFail (Error Expr Type Pattern TypeCtx) SomeReplState
  | PrintSuccess (PLPrinter.Doc) SomeReplState

main :: IO ()
main = do

  run App{..}
  where
    run :: Eq (State Name) => App (State Name) (Event Name) -> JSM ()
    run = startApp

    -- Executed on application load
    initialAction :: Event Name
    initialAction = PLEvent . EditorEv . InsertText . Test._parsesFrom . snd . (!! 0) . Map.toList $ exampleLispyTestCases

    model :: State Name
    model = State $ PL.initialState (Just EditorCursor) usage codeStore

    update :: Event Name -> State Name -> Effect (Event Name) (State Name)
    update = handleEvent

    view :: State Name -> View (Event Name)
    view = drawUI

    -- Delegated events the body will listen for
    events :: Map MisoString Bool
    events = defaultEvents

    -- Subscriptions ran during application lifetime
    subs :: [Sink (Event Name) -> JSM ()]
    subs = []

    -- Root element for DOM diff
    mountPoint :: Maybe MisoString
    mountPoint = Just "repl"

-- Backing storage is a memory cache over LocalStorage. We use this to back
-- a HashStore for expressions, allowing them to be persisted across
-- invocations of the REPL.
--
-- We use an orphan instance to serialize Exprs meaning the LocalStorage cannot
-- easily be shared. We guard against this by nesting under a '/lispy'
-- namespace.
--
-- Ideally a canonical serialization format would exist.
codeStore :: CodeStore
codeStore = newCodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
  where
    exprStore = newNestedStore newEmptyMemoryStore exprLocalStorageStore
    typeStore = newNestedStore newEmptyMemoryStore typeLocalStorageStore
    kindStore = newNestedStore newEmptyMemoryStore kindLocalStorageStore

    exprTypeStore = newNestedStore newEmptyMemoryStore exprTypeLocalStorageStore
    typeKindStore = newNestedStore newEmptyMemoryStore typeKindLocalStorageStore

    exprLocalStorageStore :: LocalStorageStore Hash Expr
    exprLocalStorageStore = newLocalStorageStore
      (\exprHash -> qualifiedKeyName ".pl/lispy/expr" exprHash <> "/reduced")
      serializeJSString
      deserializeJSString
      (==)

    typeLocalStorageStore :: LocalStorageStore Hash Type
    typeLocalStorageStore = newLocalStorageStore
      (\typeHash -> qualifiedKeyName ".pl/lispy/type" typeHash <> "/reduced")
      serializeJSString
      deserializeJSString
      (==)

    kindLocalStorageStore :: LocalStorageStore Hash Kind
    kindLocalStorageStore = newLocalStorageStore
      (\kindHash -> qualifiedKeyName ".pl/lispy/kind"  kindHash <> "/reduced")
      serializeJSString
      deserializeJSString
      (==)

    exprTypeLocalStorageStore :: LocalStorageStore Hash Hash
    exprTypeLocalStorageStore = newLocalStorageStore
      (\exprHash -> qualifiedKeyName ".pl/lispy/expr" exprHash <> "/type")
      serializeJSString
      deserializeJSString
      (==)

    typeKindLocalStorageStore :: LocalStorageStore Hash Hash
    typeKindLocalStorageStore = newLocalStorageStore
      (\typeHash -> qualifiedKeyName ".pl/lispy/type"  typeHash <> "/kind")
      serializeJSString
      deserializeJSString
      (==)

-- Hijack the Lispy Parser/ Printer to define a missing serialize instance for
-- expressions to allow us to store and retrieve them from the filesystem.
--
-- Note that this means we cannot share filestores generated with this instance
-- with anything using a different orphan instance.
--
-- TODO: Remove when Exprs gain a canonical storage format.
instance Serialize Expr where
  serialize expr = serialize $ addComments expr
  deserialize bs = stripComments <$> deserialize bs
instance Serialize CommentedExpr where
  serialize expr = case pprint (toPrinter grammar) expr of
    Nothing
      -> error "Failed to Serialize an expression via a pretty-printer"

    Just doc
      -> encodeUtf8 . PLPrinter.render $ doc
    where
      grammar :: G.Grammar CommentedExpr
      grammar = L.top $ L.expr L.var (L.sub $ L.typ L.tyVar) L.tyVar

  -- TODO: It might be nice to be able to fail with a reason when serialization
  -- is unsuccessful.
  deserialize bs = case PLParser.runParser (toParser grammar) $ decodeUtf8 bs of
    PLParser.ParseFailure _expected _cursor
      -> Nothing
    PLParser.ParseSuccess expr cursor
      | noTrailingCharacters $ PLParser.remainder cursor
      -> Just expr

      | otherwise
      -> Nothing
    where
      noTrailingCharacters :: Text -> Bool
      noTrailingCharacters txt = Text.null txt || Text.all (`elem` [' ','\t','\n','\r']) txt

      grammar :: G.Grammar CommentedExpr
      grammar = L.top $ L.expr L.var (L.sub $ L.typ L.tyVar) L.tyVar
instance Serialize Type where
  serialize typ = serialize $ addTypeComments typ
  deserialize bs = stripTypeComments <$> deserialize bs
instance Serialize CommentedType where
  serialize typ = case pprint (toPrinter grammar) typ of
    Nothing
      -> error "Failed to Serialize a type via a pretty-printer"

    Just doc
      -> encodeUtf8 . PLPrinter.render $ doc
    where
      grammar :: G.Grammar CommentedType
      grammar = L.sub $ L.typ L.tyVar

  -- TODO: It might be nice to be able to fail with a reason when serialization
  -- is unsuccessful.
  deserialize bs = case PLParser.runParser (toParser grammar) $ decodeUtf8 bs of
    PLParser.ParseFailure _expected _cursor
      -> Nothing
    PLParser.ParseSuccess typ cursor
      | noTrailingCharacters $ PLParser.remainder cursor
      -> Just typ

      | otherwise
      -> Nothing
    where
      noTrailingCharacters :: Text -> Bool
      noTrailingCharacters txt = Text.null txt || Text.all (`elem` [' ','\t','\n','\r']) txt

      grammar :: G.Grammar CommentedType
      grammar = L.sub $ L.typ L.tyVar
instance Serialize Kind where
  serialize kind = case pprint (toPrinter L.kind) kind of
    Nothing
      -> error "Failed to Serialize a kind via a pretty-printer"

    Just doc
      -> encodeUtf8 . PLPrinter.render $ doc

  -- TODO: It might be nice to be able to fail with a reason when serialization
  -- is unsuccessful.
  deserialize bs = case PLParser.runParser (toParser L.kind) $ decodeUtf8 bs of
    PLParser.ParseFailure _expected _cursor
      -> Nothing
    PLParser.ParseSuccess kind cursor
      | noTrailingCharacters $ PLParser.remainder cursor
      -> Just kind

      | otherwise
      -> Nothing
    where
      noTrailingCharacters :: Text -> Bool
      noTrailingCharacters txt = Text.null txt || Text.all (`elem` [' ','\t','\n','\r']) txt

-- | Transform the state in response to an event with optional side effects.
handleEvent :: Event Name -> State Name -> Effect (Event Name) (State Name)
handleEvent ev (State st) = case ev of
  NoOp
    -> noEff $ State $ st

  ReplaceEditorText txt
    -> noEff $ State st{PL._editorState = handleEditorEventDefault (InsertText txt ). handleEditorEventDefault Clear . PL._editorState $ st}

  RandomExpression
    -> (State st)
       <# do txt <- randomExample
             pure $ ReplaceEditorText txt

  -- Attempt to read text
  Read
    -> (State st) <# do
         let txt = PL.editorText . PL._editorState $ st
         pure $ Eval txt

  -- Attempt to evaluate text
  Eval txt
    -> (State st) <# do
         (someReplState',eRes) <- case PL._replState st of
                                      SomeReplState replState
                                        -> do let step = replStep txt
                                              (nextState, result) <- _unRepl step replState
                                              pure (SomeReplState nextState, result)
         case eRes of
             Left err
               -> pure $ PrintFail err someReplState'
             Right a
               -> pure $ PrintSuccess a someReplState'

  -- Failed to evaluate text
  PrintFail err newReplState
    -> let ppType = fromMaybe mempty . pprint (toPrinter $ top $ typ tyVar) . addTypeComments
           ppPattern = fromMaybe mempty . pprint (toPrinter $ top $ pattern var tyVar) . addPatternComments
           ppExpr = fromMaybe mempty . pprint (toPrinter $ top $ expr var (top $ typ tyVar) tyVar) . addComments
           ppVar     = fromMaybe mempty . pprint (toPrinter var)
           ppTyVar   = fromMaybe mempty . pprint (toPrinter tyVar)
        in noEff $ State st{ PL._replState    = newReplState
                           , PL._outputState  = PL.newOutputState . Text.lines . PLPrinter.render . PL.ppError ppPattern ppType ppExpr (ppTypeCtx document (ppTypeInfo ppType)) ppVar ppTyVar $ err
                           , PL._typeCtxState = PL.typeCtxStateGivenReplState newReplState
                           , PL._focusOn      = Just OutputCursor
                           }

  -- Successfully evaluated text
  PrintSuccess a newReplState
    -> noEff $ State  st{ PL._replState    = newReplState
                        , PL._editorState  = PL.emptyEditorState
                        , PL._outputState  = PL.newOutputState $ Text.lines $ PLPrinter.renderDocument a
                        , PL._typeCtxState = PL.typeCtxStateGivenReplState newReplState
                        , PL._focusOn      = Just EditorCursor
                        }

  -- Events to core internal widgets
  PLEvent plEv
    -> case plEv of
         -- Replace the entire repl state
         ReplaceReplState someReplState'
          -> noEff $ State $ st{PL._replState = someReplState'}

         -- An event for the editor
         EditorEv editorEv
          -> noEff $ State $ st{PL._editorState = handleEditorEventDefault editorEv $ PL._editorState st}

         -- An event for the output
         OutputEv outputEv
          -> noEff $ State $ st{PL._outputState = handleOutputEventDefault outputEv $ PL._outputState st}

         -- An event for the type context
         TypeCtxEv typeCtxEv
          -> noEff $ State $ st{PL._typeCtxState = handleTypeCtxEventDefault typeCtxEv $ PL._typeCtxState st}

         -- An event for the usage
         UsageEv usageEv
          -> noEff $ State $ st{PL._usageState = handleUsageEventDefault usageEv $ PL._usageState st}

         -- Switch input focus
         FocusOn n
          -> noEff $ State $ st{PL._focusOn = n}

-- Draw the entire state
drawUI :: State Name -> View (Event Name)
drawUI (State st) = div_
  [ id_ "ui"
  , style_ $ Map.fromList $
      [("display","flex")
      ]
  ]

  [ div_
      [ id_ "io-widgets"]
      [ div_
          [ id_ "editor-form"]
          [ drawEditor EditorCursor (PL._editorState st)
          , input_
              [ type_  "submit"
              , value_ "Evaluate"
              , onClick Read
              ]
          , input_
              [ type_ "submit"
              , value_ "Random expression"
              , onClick RandomExpression
              ]
          ]
      , drawOutput OutputCursor (PL._outputState st)
      ]

  , div_
      [ id_ "context-widgets"
      ]
      [ drawTypeCtx TypeCtxCursor (case PL._replState st of
                                     SomeReplState replState -> _typeCtx . _typeCheckCtx $ replState
                                  )
      , drawUsage UsageCursor (PL._usageState st)
      ]
  ]
  where

  -- TODO: Set view to size of output DOM elements/ add scrolling
  drawEditor
    :: Name
    -> PL.EditorState
    -> View (Event Name)
  drawEditor _editorCursor editor =
    let txt = PL.editorText editor
     in div_
          [ id_ "editor"]
          [ h2_ [] [ text "Editor"]
          , textarea_
              [ id_ "editor-input"
              , autofocus_ True
              , cols_ "80"
              , rows_ "20"
              , value_ (ms txt)
              , onChange (ReplaceEditorText . Text.pack . fromMisoString)
              ]
              []
          ]

  drawOutput
    :: Name
    -> PL.OutputState
    -> View (Event Name)
  drawOutput _outputCursor editor =
    let txt = PL.editorText editor
     in div_
          [ id_ "output"
          ]
          [ h2_ [] [ text "Output"]
          , output_
             [ id_ "output-text"
             , for_ "editor-form"
             ]
             [ pre_ [] [text . ms $ txt]]
          ]

  drawTypeCtx
    :: Name
    -> TypeCtx
    -> View (Event Name)
  drawTypeCtx _typCtxCursor typeCtx =
    let ppType = fromMaybe mempty . pprint (toPrinter $ top $ typ tyVar) . addTypeComments
        txt = (PLPrinter.render . ppTypeCtx document (ppTypeInfo ppType)) $ typeCtx
     in div_
          [ id_ "type-ctx"
          ]
          [ h2_ [] [ text "Type context" ]
          , p_
              [ id_ "type-ctx-types"
              ]
              [ pre_ [] [text . ms $ txt]]
          ]

  drawUsage
    :: Name
    -> PL.UsageState
    -> View (Event Name)
  drawUsage _usageCursor editor =
    let txt = PL.editorText editor
     in div_
          [ id_ "usage"
          ]
          [ h2_ [] [ text "Usage"]
          , p_
              [ id_ "usage"
              ]
              [ text "Type PL expressions in the editor (which may reference the built in types) using the Lispy syntax. Evaluating will display in the output pane:"
              , ul_ []
                  [ li_ [] [text "Parse errors"]
                  , li_ [] [text "Type errors"]
                  , li_ [] [text "The inferred type"]
                  , li_ [] [text "The reduction"]
                  ]
              ]
          ]

-- TODO: We could share text fragments with the terminal ui if:
-- - We used PLPrinter Docs
-- - Defined a render function which renders with <br> rather than newlines
-- - Taught Doc about bullet points
usage :: [Text]
usage =
  [ "Type PL expressions in the editor (which may reference the built in types) using the Lispy syntax. Evaluating will display in the output pane:"
  , "- Parse errors"
  , "- Type errors"
  , "- The infered type"
  , "- The reduction"
  ]

-- Generate random example from the Lispy implementation of the PL test cases
randomExample :: IO Text
randomExample = do
  let exampleNames = Map.keys exampleLispyTestCases
      nExamples    = Prelude.length exampleNames
  randomIndex <- randomRIO (0,nExamples - 1)
  let randomName = exampleNames !! randomIndex
      Just randomTestCase = Map.lookup randomName exampleLispyTestCases
  return . Test._parsesFrom $ randomTestCase

exampleLispyTestCases :: Map Text Test.ExprTestCase
exampleLispyTestCases = Test.mkTestCases Test.sources


