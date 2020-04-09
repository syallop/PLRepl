{-# LANGUAGE
    FlexibleContexts
  , ImplicitParams
  , OverloadedStrings
  , TemplateHaskell
  #-}
{-|
Module      : Main
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

The main module provides a cli repl executable for the PL language.

Our cli widgets are found under PLRepl.Widgets/WIDGET underwhich by convention:
  - WIDGET.Event defines the events the widget emits/ understands.
  -
  - WIDGET.Name defines names used to reference components of the widget, such
    as its cursors or viewport.

  - WIDGET.State defines the state of the widget. Often a drawWIDGET function
    will be exported to draw this function as a renderable widget.

In particular:
  - Editor: An editable text editor, build upon PLEditor.
    A use case is entering source code.

  - Output: An output text area. This is currently an alias to Editor
    where we expect editing events not to be forwarded.
    A use case is responses to parsing/ type checking source code entered in an editor.

  - TypeCtx: An output area containing defined type-definitions.

  - Usage: An output area containing usage information

PLRepl.Repl abstracts the Read Eval Print Loop for some PL repl.
It accepts a repl configuration which it understands how to drive.

Repl configuration(s) are found under PLRepl.Repl.* This abstraction allows
defining multiple repls which can be switched between, for example we might
want a repl for different expression grammars or a repl exclusively for type
signatures.

-}
module Main where

import PLRepl.Repl          as PL
import PLRepl.Repl.Lispy    as PL
import PLRepl.Widgets.Event as PL
import PLRepl.Widgets.Name  as PL
import PLRepl.Widgets.State as PL

import PLLispy
import PL.TyVar
import PL.Type
import PL.Var
import PL.Error
import PL.TypeCtx
import PL.Kind
import PL.Name

import PL.Test.Source
import qualified PL.Test.Expr as Test
import qualified PL.Test.ExprTestCase as Test
import qualified PLLispy.Test.Sources.Expr as Test

import PLGrammar

import PLPrinter
import PLPrinter.Doc

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Graphics.Vty as Vty
import qualified Brick as Brick

import Control.Concurrent (threadDelay, forkIO, forkFinally)
import Control.Arrow (first)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.List (isPrefixOf)
import Data.Text (Text)
import System.Directory
import System.Exit
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import Data.Maybe

-- | The ReplApp is a Brick App which handles `Events` to update `State`
-- making use of `Name`'s to name resources.
type ReplApp = App (PL.State PL.Name) (PL.Event PL.Name) PL.Name

-- | replApp is a Brick app providing a repl for PL.
replApp
  :: BChan (PL.Event PL.Name)
  -> ReplApp
replApp chan = App
  { -- No actions needed on startup.
    appStartEvent = pure

    -- Update the repl state in response to events.
  , appHandleEvent = handleEvent chan

    -- Always pick the Editor widget for the cursor.
  , appChooseCursor = \st -> case _focusOn st of
                               Nothing
                                 -> const Nothing
                               Just n
                                 -> showCursorNamed n

    -- Named attributes describing reusable layout and drawing properties.
  , appAttrMap = const attributes

    -- Convert the state to a list of widgets that may be drawn.
  , appDraw = drawUI
  }

-- | Update the repl state in response to events.
handleEvent
  :: BChan (PL.Event PL.Name)
  -> PL.State PL.Name
  -> BrickEvent PL.Name (PL.Event PL.Name)
  -> EventM PL.Name (Next (PL.State PL.Name))
handleEvent chan (st@(PL.State someReplState replConfigs editorSt outputSt typeCtxSt usageSt focus)) ev = case ev of
  -- an event from our application
  AppEvent appEv -> case appEv of
    -- Replctx must be updated
    PL.ReplaceReplState someReplState'
      -> continue (PL.State someReplState' replConfigs editorSt outputSt typeCtxSt usageSt focus)

    -- An event to the editor
    PL.EditorEv editorEv
      -> do editorSt' <- handleEditorEvent editorEv editorSt
            continue (PL.State someReplState replConfigs editorSt' outputSt typeCtxSt usageSt focus)

    PL.OutputEv outputEv
      -> do outputSt' <- handleOutputEvent outputEv outputSt
            continue (PL.State someReplState replConfigs editorSt outputSt' typeCtxSt usageSt focus)

    PL.TypeCtxEv typeCtxEv
      -> do typeCtxSt' <- handleTypeCtxEvent typeCtxEv typeCtxSt
            continue (PL.State someReplState replConfigs editorSt outputSt typeCtxSt' usageSt focus)

    PL.UsageEv usageEv
      -> do usageSt' <- handleUsageEvent usageEv usageSt
            continue (PL.State someReplState replConfigs editorSt outputSt typeCtxSt usageSt' focus)

    PL.FocusOn n
      -> continue (PL.State someReplState replConfigs editorSt outputSt typeCtxSt usageSt n)

  -- A virtual terminal event. Most events will be sent to whatever widget we
  -- consider focused. Some may be global.
  VtyEvent vtyEv -> case vtyEv of
    -- A key with no modifiers
    Vty.EvKey keyEv modifiers -> case keyEv of

      -- up arrow => cursor up
      -- +ctrl    => taller view
      Vty.KUp -> case modifiers of
        []
          -> sendToFocused chan CursorUp focus >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (TallerView 1) focus >> continue st
        _ -> continue st

      -- down arrow => cursor down
      -- +ctrl      => shorter view
      Vty.KDown -> case modifiers of
        []
          -> sendToFocused chan CursorDown focus >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (TallerView (-1)) focus >> continue st
        _ -> continue st

      -- left arrow => cursor left
      -- +ctrl      => narrower view
      Vty.KLeft -> case modifiers of
        []
          -> sendToFocused chan CursorLeft focus >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (WiderView (-1)) focus >> continue st
        _ -> continue st

      -- right arrow => cursor right
      -- +ctrl       => wider view
      Vty.KRight -> case modifiers of
        []
          -> sendToFocused chan CursorRight focus >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (WiderView 1) focus >> continue st
        _ -> continue st

      -- any character => insert that character in the editor.
      -- TODO: Should this still happen when another widget has focus?
      Vty.KChar c
        -> case modifiers of
             []
               -> liftIO (writeBChan chan . EditorEv . InsertChar $ c) >> continue st
             _ -> continue st

      -- delete => delete a character in the editor.
      -- TODO: Should this still happen when another widget has focus?
      Vty.KDel
        -> do liftIO (writeBChan chan . EditorEv $ CursorRight)
              liftIO (writeBChan chan . EditorEv $ DeleteChar)
              continue st
      Vty.KBS
        -> liftIO (writeBChan chan . EditorEv $ DeleteChar) >> continue st

      -- home => insert a random code fragment in the editor.
      Vty.KHome
        -> do randomCode <- liftIO $ randomExample
              liftIO (writeBChan chan . EditorEv . InsertText $ randomCode)
              continue st

      -- enter => insert a newline in the editor.
      -- TODO: Should this still happen when another widget has focus?
      Vty.KEnter -> case modifiers of
        []
          -> liftIO (writeBChan chan . EditorEv $ NewLine) >> continue st
        _ -> continue st

      -- insert => grab the text in the editor and run it through the configured
      -- repl.
      --   - on failure => render the error in the output widget and switch
      --     focus there.
      --   - on success => render the parse in the output widget and switch
      --     focus to the editor.
      Vty.KIns
        -> do let txt             = editorText editorSt
              let ?eb             = var
                  ?abs            = typ tyVar
                  ?tb             = tyVar
              let (someReplState',eRes) = case someReplState of
                                            SomeReplState replState
                                              -> let step = PL.replStep txt
                                                     (nextState, result) = _unRepl step replState
                                                  in (SomeReplState nextState, result)
              case eRes of
                -- Some repl error
                Left err
                  -- TODO:
                  -- - Printer should be able to render a document to a list
                  --   of lines with and without a DocFmt to prevent
                  --   re-detecting the newlines.
                  -- - The printer should be passed the current width so it
                  --   wraps optimally.
                  -> continue (PL.State someReplState
                                        replConfigs
                                        editorSt
                                        (newOutputState $ Text.lines $ (PLPrinter.render . ppError tyVar) err)
                                        (typeCtxStateGivenReplState someReplState)
                                        usageSt
                                        (Just OutputCursor))

                -- A successful parse
                Right a
                  -> do liftIO (writeBChan chan . ReplaceReplState $ someReplState')
                        continue (PL.State someReplState'
                                           replConfigs
                                           emptyEditorState
                                           (newOutputState $ Text.lines $ renderDocument a)
                                           (typeCtxStateGivenReplState someReplState)
                                           usageSt
                                           (Just EditorCursor))

      -- page-up => switch focus to the next widget.
      Vty.KPageUp
        -> liftIO (writeBChan chan $ FocusOn $ fmap nextFocus $ focus) >> continue st

      -- page-down => switch focus to the previous widget.
      Vty.KPageDown
        -> liftIO (writeBChan chan . FocusOn . fmap previousFocus $ focus) >> continue st

      -- escape => exit the program.
      Vty.KEsc
        -> liftIO $ exitSuccess

      _ -> continue st

    _ -> continue st

  _ -> continue st

-- | If an editor like thing is focused, send it an event.
sendToFocused
  :: BChan (PL.Event PL.Name)
  -> EditorEvent
  -> Maybe PL.Name
  -> EventM PL.Name ()
sendToFocused chan event = maybe (pure ()) (\focus -> liftIO $ writeBChan chan . eventDestination focus $ event)

-- | Given a focus name, decide which editor like thing should recieve an event.
eventDestination :: PL.Name -> EditorEvent -> PL.Event PL.Name
eventDestination focusOn = case focusOn of
  EditorCursor  -> EditorEv
  OutputCursor  -> OutputEv
  TypeCtxCursor -> TypeCtxEv
  UsageCursor   -> UsageEv
  _ -> EditorEv

-- | Named attributes describing reusable layout and drawing properties.
attributes :: AttrMap
attributes = attrMap defAttr []

-- | Convert the state to a list of widgets that may be drawn.
-- Something like:
--
-- |--------|---------|
-- |        |         |
-- | editor | sidebar |
-- |--------|         |
-- | output |         |
-- |--------|---------|
drawUI
  :: PL.State PL.Name
  -> [Widget PL.Name]
drawUI st =
  [ center $ everything
  ]
  where
    -- |-------------|--------|
    -- |             | types  |
    -- | editor      |        |
    -- |-------------|--------|
    -- | output      | usage  |
    -- |-------------|--------|
    everything :: Widget PL.Name
    everything = hLimitPercent 60 mainWidgets <+> sidebar

    -- |--------|
    -- |        |
    -- | editor |
    -- |--------|
    -- | output |
    -- |--------|
    mainWidgets :: Widget PL.Name
    mainWidgets = editor <=> output

    -- |--------|
    -- |        |
    -- | editor |
    -- |--------|
    editor :: Widget PL.Name
    editor = borderWithLabel (str "Editor")
           $ padRight Max
           $ padBottom Max
           $ drawEditor EditorCursor (_editorState st)

    -- |--------|
    -- | output |
    -- |--------|
    output :: Widget PL.Name
    output = borderWithLabel (str "Output")
           $ padRight Max
           $ padBottom Max
           $ drawOutput OutputCursor (_outputState st)

    -- |---------|
    -- |  types  |
    -- |         |
    -- |---------|
    -- |  usage  |
    -- |---------|
    sidebar :: Widget PL.Name
    sidebar = types <=> usage

    types :: Widget PL.Name
    types = borderWithLabel (str "Built in types")
          $ padRight Max
          $ padBottom Max
          $ drawTypeCtx TypeCtxCursor (_typeCtxState st)

    usage :: Widget PL.Name
    usage = borderWithLabel (str "Usage")
          $ padRight Max
          $ padBottom Max
          $ drawUsage UsageCursor (_usageState st)

main :: IO ()
main = run

run :: IO ()
run = do
  -- Buffer events
  evChan <- newBChan 10
  void $ customMain (Vty.mkVty defaultConfig) (Just evChan) (replApp evChan) (initialState (Just EditorCursor) usage)

usage :: [Text]
usage =
  [ "Type PL expressions in the editor (which may reference the built in types) using the Lispy syntax. Evaluating will display in the output pane:"
  , "- Parse errors"
  , "- Type errors"
  , "- The infered type"
  , "- The reduction"
  , ""
  , "Keys:"
  , "Evaluate       : INS"
  , "Focus pane     : Pg Up/Down"
  , "Random example : HOME"
  , ""
  , "Exit       : ESC"
  ]

-- Generate random example from the Lispy implementation of the PL test cases
randomExample :: IO Text
randomExample = do
  let exampleNames = Map.keys exampleLispyTestCases
      nExamples    = length exampleNames
  randomIndex <- randomRIO (0,nExamples - 1)
  let randomName = exampleNames !! randomIndex
      Just randomTestCase = Map.lookup randomName exampleLispyTestCases
  return . Test._parsesFrom $ randomTestCase

exampleLispyTestCases :: Map Text Test.ExprTestCase
exampleLispyTestCases = Map.fromList $ Test.testCases Test.sources

