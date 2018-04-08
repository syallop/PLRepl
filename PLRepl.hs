{-# LANGUAGE
    FlexibleContexts
  , ImplicitParams
  , OverloadedStrings
  , TemplateHaskell
  #-}
module Main where

import PLRepl.Repl as PL
import PLRepl.Repl.Lispy as PL
import PLRepl.Widgets.Event as PL
import PLRepl.Widgets.Name as PL
import PLRepl.Widgets.State as PL

import PLLispy
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
handleEvent chan (st@(PL.State someReplState replConfigs editorSt outputSt typeCtxSt focus)) ev = case ev of
  -- an event from our application
  AppEvent appEv -> case appEv of
    -- Replctx must be updated
    PL.ReplaceReplState someReplState'
      -> continue (PL.State someReplState' replConfigs editorSt outputSt typeCtxSt focus)

    -- An event to the editor
    PL.EditorEv editorEv
      -> do editorSt' <- handleEditorEvent editorEv editorSt
            continue (PL.State someReplState replConfigs editorSt' outputSt typeCtxSt focus)

    PL.OutputEv outputEv
      -> do outputSt' <- handleOutputEvent outputEv outputSt
            continue (PL.State someReplState replConfigs editorSt outputSt' typeCtxSt focus)

    PL.TypeCtxEv typeCtxEv
      -> do typeCtxSt' <- handleTypeCtxEvent typeCtxEv typeCtxSt
            continue (PL.State someReplState replConfigs editorSt outputSt typeCtxSt' focus)

    PL.FocusOn n
      -> continue (PL.State someReplState replConfigs editorSt outputSt typeCtxSt n)

  -- A virtual terminal event
  VtyEvent vtyEv -> case vtyEv of
    -- A key with no modifiers
    Vty.EvKey keyEv modifiers -> case keyEv of
      Vty.KUp -> case modifiers of
        []
          -> sendToFocused chan CursorUp focus >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (TallerView 1) focus >> continue st
        _ -> continue st

      Vty.KDown -> case modifiers of
        []
          -> sendToFocused chan CursorDown focus >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (TallerView (-1)) focus >> continue st
        _ -> continue st

      Vty.KLeft -> case modifiers of
        []
          -> sendToFocused chan CursorLeft focus >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (WiderView (-1)) focus >> continue st
        _ -> continue st

      Vty.KRight -> case modifiers of
        []
          -> sendToFocused chan CursorRight focus >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (WiderView 1) focus >> continue st
        _ -> continue st

      Vty.KChar c
        -> liftIO (writeBChan chan . EditorEv . InsertChar $ c) >> continue st

      Vty.KDel
        -> liftIO (writeBChan chan . EditorEv $ DeleteChar) >> continue st

      Vty.KEnter -> case modifiers of
        []
          -> liftIO (writeBChan chan . EditorEv $ NewLine) >> continue st
        _ -> continue st

      Vty.KIns
        -> do let txt             = editorText editorSt
              let ?eb             = var
                  ?abs            = typ tyVar
                  ?tb             = tyVar
              let (someReplState',eRes) = case someReplState of
                                            SomeReplState replState
                                              -> first SomeReplState . (`_unRepl` replState) . PL.replStep . editorText $ editorSt
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
                                        (newOutputState $ Text.lines $ renderDocument err)
                                        (case someReplState of
                                           SomeReplState replState
                                             -> newTypeCtxState . Text.lines . renderDocument . _typeCtx $ replState
                                        )
                                        (Just OutputCursor))

                -- A successful parse
                Right a
                  -> do liftIO (writeBChan chan . ReplaceReplState $ someReplState')
                        continue (PL.State someReplState'
                                           replConfigs
                                           emptyEditorState
                                           (newOutputState $ Text.lines $ renderDocument a)
                                           (case someReplState' of
                                              SomeReplState replState'
                                                -> newTypeCtxState . Text.lines . renderDocument . _typeCtx $ replState'
                                           )
                                           (Just EditorCursor))

      Vty.KPageUp
        -> liftIO (writeBChan chan $ FocusOn $ fmap nextFocus $ focus) >> continue st

      Vty.KPageDown
        -> liftIO (writeBChan chan . FocusOn . fmap previousFocus $ focus) >> continue st

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
  _ -> EditorEv

-- | Named attributes describing reusable layout and drawing properties.
attributes :: AttrMap
attributes = attrMap defAttr []

-- | Convert the state to a list of widgets that may be drawn.
drawUI
  :: PL.State PL.Name
  -> [Widget PL.Name]
drawUI st =
  [ center $ border $ hLimit 200 $ vLimit 50 $ everything
  ]
  where
    everything :: Widget PL.Name
    everything = mainWidgets <+> sidebar

    mainWidgets :: Widget PL.Name
    mainWidgets = editor <=> output

    editor :: Widget PL.Name
    editor = hLimit 160 $ border $ viewport EditorViewport Vertical $ vLimit 48 $ drawEditor EditorCursor (_editorState st)

    output :: Widget PL.Name
    output  = hLimit 160 $ border $ viewport OutputViewport Horizontal $ vLimit 100 $ drawOutput OutputCursor (_outputState st)

    sidebar :: Widget PL.Name
    sidebar = hLimit 40 $ border $ viewport TypeCtxViewport Horizontal $ vLimit 20 $ drawTypeCtx TypeCtxCursor (_typeCtxState st)

main :: IO ()
main = run

run :: IO ()
run = do
  -- Buffer events
  evChan <- newBChan 10
  void $ customMain (Vty.mkVty defaultConfig) (Just evChan) (replApp evChan) (initialState $ Just EditorCursor)

