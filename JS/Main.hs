{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

-- PL dependencies
import qualified PL as PL

import qualified PL.Expr as PL
import PL.Expr hiding (Expr, App)

import qualified PL.Type as PL
import PL.Type hiding (Type)

import PLRepl.Repl
import qualified PLRepl.Widgets.State as PL
import PLRepl.Widgets.Name
import PLRepl.Widgets.Event

import qualified PL.Test.Expr as Test
import qualified PL.Test.ExprTestCase as Test
import qualified PLLispy.Test.Sources.Expr as Test

import PL.Var
import PL.TyVar
import PL.FixExpr
import PL.FixType

import qualified PLEditor as E

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

main :: IO ()
main = run App{..}
  where
    run :: Eq (State Name) => App (State Name) (Event Name) -> JSM ()
    run = startApp

    -- Executed on application load
    initialAction :: Event Name
    initialAction = EditorEv . InsertText . Test._parsesFrom . snd . (!! 0) . Map.toList $ exampleLispyTestCases

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
drawUI (State st) = div_ [] $ [
  text "Input:"
  ] <> [drawEditor EditorCursor (PL._editorState st)]
  where

  drawEditor
    :: Name
    -> PL.EditorState
    -> View (Event Name)
  drawEditor editorCursor (PL.EditorState editor view) = div_ [] [ text . ms . (\(lines,pos) -> Text.unlines . fmap E.lineText . E.renderLines $ lines) . E.viewEditor view $ editor ]

usage :: [Text]
usage =
  ["Usage"
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

