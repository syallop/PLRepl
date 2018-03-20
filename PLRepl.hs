{-# LANGUAGE
    FlexibleContexts
  , ImplicitParams
  , OverloadedStrings
  , TemplateHaskell
  #-}
module Main where

import PLRepl.Event as PL
import PLRepl.Name as PL
import PLRepl.State as PL

import PL.Grammar.Lispy
import PL.Repl
import PL.TyVar
import PL.Type
import PL.Var
import PLPrinter

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Graphics.Vty as Vty
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import qualified System.Console.Haskeline.Brick as HL

import Control.Concurrent (threadDelay, forkIO, forkFinally)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.List (isPrefixOf)
import Data.Text (Text)
import System.Directory
import qualified Data.Text as Text

-- | The ReplApp is a Brick App which handles `Events` to update `State`
-- making use of `Name`'s to name resources.
type ReplApp = App PL.State PL.Event PL.Name

-- | replApp is a Brick app providing a repl for PL.
replApp
  :: BChan PL.Event
  -> ReplApp
replApp chan = App
  { -- No actions needed on startup.
    appStartEvent = pure

    -- Update the repl state in response to events.
  , appHandleEvent = handleEvent chan

    -- Always pick the Editor widget for the cursor.
  , appChooseCursor = \_ -> showCursorNamed EditorWidget

    -- Named attributes describing reusable layout and drawing properties.
  , appAttrMap = const attributes

    -- Convert the state to a list of widgets that may be drawn.
  , appDraw = drawUI
  }

-- | Update the repl state in response to events.
handleEvent
  :: BChan PL.Event
  -> PL.State
  -> BrickEvent PL.Name PL.Event
  -> EventM PL.Name (Next PL.State)
handleEvent chan (st@(PL.State replCtx editorSt outputSt)) ev = case ev of
  -- an event from our application
  AppEvent appEv -> case appEv of
    -- Replctx must be updated
    PL.ReplaceReplCtx replCtx'
      -> continue (PL.State replCtx' editorSt outputSt)

    -- An event to the editor
    PL.EditorEv editorEv
      -> do editorSt' <- handleEditorEvent editorEv editorSt
            continue (PL.State replCtx editorSt' outputSt)

    PL.OutputEv outputEv
      -> do outputSt' <- handleOutputEvent outputEv outputSt
            continue (PL.State replCtx editorSt outputSt')

  -- A virtual terminal event
  VtyEvent vtyEv -> case vtyEv of
    -- A key with no modifiers
    Vty.EvKey keyEv modifiers -> case keyEv of
      Vty.KUp -> case modifiers of
        []
          -> liftIO (writeBChan chan . EditorEv $ CursorUp) >> continue st
        [Vty.MCtrl]
          -> liftIO (writeBChan chan . EditorEv . TallerView $ 1)  >> continue st

      Vty.KDown -> case modifiers of
        []
          -> liftIO (writeBChan chan . EditorEv $ CursorDown)     >> continue st
        [Vty.MCtrl]
          -> liftIO (writeBChan chan . EditorEv . TallerView $ -1) >> continue st

      Vty.KLeft -> case modifiers of
        []
          -> liftIO (writeBChan chan . EditorEv $ CursorLeft)     >> continue st
        [Vty.MCtrl]
          -> liftIO (writeBChan chan . EditorEv . WiderView $ -1)  >> continue st

      Vty.KRight -> case modifiers of
        []
          -> liftIO (writeBChan chan . EditorEv $ CursorRight)    >> continue st
        [Vty.MCtrl]
          -> liftIO (writeBChan chan . EditorEv . WiderView $ 1)   >> continue st

      Vty.KChar c
        -> liftIO (writeBChan chan . EditorEv . InsertChar $ c) >> continue st

      Vty.KDel
        -> liftIO (writeBChan chan . EditorEv $ DeleteChar)     >> continue st

      Vty.KEnter -> case modifiers of
        []
          -> liftIO (writeBChan chan . EditorEv $ NewLine)        >> continue st

      Vty.KIns
        -> do let txt             = editorText editorSt
              let ?eb             = var
                  ?abs            = typ tyVar
                  ?tb             = tyVar
              let (replCtx',eRes) = (\r -> _unRepl r replCtx) . replStep var (typ tyVar) tyVar . editorText $ editorSt
              case eRes of
                -- Some repl error
                Left err
                  -> continue (PL.State replCtx editorSt (newOutputState $ [renderDocument err]))

                -- A successful parse
                Right a
                  -> do liftIO (writeBChan chan . ReplaceReplCtx $ replCtx')
                        continue (PL.State replCtx' emptyEditorState $ newOutputState $ [renderDocument a])

      _ -> continue st

      _ -> continue st

  _ -> continue st

-- | Named attributes describing reusable layout and drawing properties.
attributes :: AttrMap
attributes = attrMap defAttr []

-- | Convert the state to a list of widgets that may be drawn.
drawUI
  :: PL.State
  -> [Widget PL.Name]
drawUI st =
  [ center $ border $ hLimit 200 $ vLimit 50 $ (editor <=> output) <+> sidebar
  ]
  where
    editor  = border $ viewport EditorViewport Vertical $ hLimit 160 $ vLimit 48 $ drawEditor (_editorState st)
    output  = border $ viewport OutputViewport Horizontal $ hLimit 160 $ vLimit 40 $ drawOutput (_outputState st)
    sidebar = border $ hLimit 60 $ viewport Sidebar Vertical $ vBox $ map (str . show) ["Sidebar"]


main :: IO ()
main = run

run :: IO ()
run = do
  -- Buffer events
  evChan <- newBChan 10
  void $ customMain (Vty.mkVty defaultConfig) (Just evChan) (replApp evChan) initialState

