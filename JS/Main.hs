{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

import GHCJS.Prim
import GHCJS.Types
import GHCJS.Marshal

-- PL dependencies
import qualified PL as PL

import qualified PL.Expr as PL
import PL.Expr hiding (Expr, App)

import qualified PL.Type as PL
import PL.Type hiding (Type)

import PLRepl.Repl
import PLRepl.Repl.Lispy
import qualified PLRepl.Widgets.State as PL
import PLRepl.Widgets.Name

import PLRepl.Widgets.Event hiding (Event)
import qualified PLRepl.Widgets.Event as PL

import qualified PL.Test.Expr as Test
import qualified PL.Test.ExprTestCase as Test
import qualified PLLispy.Test.Sources.Expr as Test

import PLLispy

import PL.Var
import PL.Error
import PL.TyVar
import PL.FixExpr
import PL.FixType

import qualified PLEditor as E

import qualified PLPrinter


-- Other dependencies
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map

import System.Random

-- Aliases for the concrete expression/ types we're going to use
type Expr = PL.Expr Var Type TyVar
type Type = PL.Type TyVar

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
  | PrintFail (Error TyVar) (SomeReplState Var Type TyVar)
  | PrintSuccess (PLPrinter.Doc) (SomeReplState Var Type TyVar)

main :: IO ()
main = run App{..}
  where
    run :: Eq (State Name) => App (State Name) (Event Name) -> JSM ()
    run = startApp

    -- Executed on application load
    initialAction :: Event Name
    initialAction = PLEvent . EditorEv . InsertText . Test._parsesFrom . snd . (!! 0) . Map.toList $ exampleLispyTestCases

    model :: State Name
    model = State $ PL.initialState (Just EditorCursor) usage

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
    mountPoint = Nothing

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
    -> let (someReplState',eRes) = case PL._replState st of
                                   SomeReplState replState
                                     -> let step = replStep txt
                                            (nextState, result) = _unRepl step replState
                                         in (SomeReplState nextState, result)
        in case eRes of
             Left err
               -> (State st) <# (pure $ PrintFail err someReplState')
             Right a
               -> (State st) <# (pure $ PrintSuccess a someReplState')

  -- Failed to evaluate text
  PrintFail err newReplState
    -> noEff $ State st{ PL._replState    = newReplState
                       , PL._outputState  = PL.newOutputState $ Text.lines $ (PLPrinter.render . PL.ppError tyVar) err
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
drawUI (State st) = div_ [] $
  [ drawUsage UsageCursor (PL._usageState st)
  , drawTypeCtx TypeCtxCursor (PL._typeCtxState st)
  , div_
      [id_ "editor-form"]
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
      , drawOutput OutputCursor (PL._outputState st)
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
    -> PL.TypeCtxState
    -> View (Event Name)
  drawTypeCtx _typCtxCursor editor =
    let txt = PL.editorText editor
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
exampleLispyTestCases = Map.fromList $ Test.testCases Test.sources

