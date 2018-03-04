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
  :: HL.Config PL.Event
  -> ReplApp
replApp config = App
  { -- No actions needed on startup.
    appStartEvent = pure

    -- Update the repl state in response to events.
  , appHandleEvent = handleEvent config

    -- Always pick the Haskeline widget for the cursor.
  , appChooseCursor = \_ -> showCursorNamed Haskeline

    -- Named attributes describing reusable layout and drawing properties.
  , appAttrMap = const attributes

    -- Convert the state to a list of widgets that may be drawn.
  , appDraw = drawUI
  }

-- | Update the repl state in response to events.
handleEvent
  :: HL.Config PL.Event
  -> PL.State
  -> BrickEvent PL.Name PL.Event
  -> EventM PL.Name (Next PL.State)
handleEvent config (PL.State replCtx hl) ev = do
  hl' <- HL.handleEvent config hl ev
  case ev of
    AppEvent aEv
      -> case aEv of
           PL.ReplaceReplCtx replCtx'
             -> continue (PL.State replCtx' hl')

           PL.HaskelineDied _eException
             -> halt(PL.State replCtx hl')

           _ -> continue (PL.State replCtx hl')

    _ -> continue (PL.State replCtx hl')

-- | Named attributes describing reusable layout and drawing properties.
attributes :: AttrMap
attributes = attrMap defAttr []

-- | Convert the state to a list of widgets that may be drawn.
drawUI
  :: PL.State
  -> [Widget PL.Name]
drawUI st =
  [ hl <+> sidebar
  ]
  where
    hl      = border $ hLimit 160 $ HL.render (_haskeline st)
    sidebar = border $ hLimit 60 $ viewport Sidebar Vertical $ vBox $ map (str . show) ["Sidebar"]


main :: IO ()
main = run

run :: IO ()
run = do
  -- Buffer events
  evChan <- newBChan 10

  -- Haskeline configuration
  hlConfig <- HL.configure evChan PL.HaskelineEv $ \ev ->
    case ev of
      PL.HaskelineEv hlEv
        -> Just hlEv
      _ -> Nothing

  -- Run haskeline, emiting the died event to Brick on death.
  void $ forkFinally (runInput hlConfig evChan)
                     (writeBChan evChan . HaskelineDied)

  -- Run Brick
  void $ customMain (Vty.mkVty defaultConfig) (Just evChan) (replApp hlConfig) initialState

-- | Repeatedly Read Eval Print Loop in haskeline, emiting events to brick.
runInput
  :: HL.Config PL.Event
  -> BChan PL.Event
  -> IO ()
runInput hlConfig evChan = do
  hlSettings <- getHaskelineSettings

  -- The Loop threads a ReplCtx as state through the InputT transformer emiting
  -- updates to Brick when it is replaced.
  flip evalStateT emptyReplCtx . runInputTBehavior (HL.useBrick hlConfig) hlSettings $ loop
  where
    myReplStep :: Text -> Repl Var (Type TyVar) TyVar Text
    myReplStep =
      let ?eb  = var
          ?abs = typ tyVar
          ?tb  = tyVar
       in replStep var (typ tyVar) tyVar

    loop :: InputT (StateT (ReplCtx Var TyVar) IO) ()
    loop = do
      mInput <- promptInput
      case mInput of
        -- End of input. Haskeline is done.
        Nothing
          -> return ()

        -- Some unparsed input.
        Just txt
          -> do -- Acquire the current replctx
                replCtx0 <- lift get

                -- Execute a repl step on the input under the context, returning
                -- the updated context and possible result.
                let (replCtx1,eRes) = (_unRepl $ myReplStep (Text.pack txt)) replCtx0
                case eRes of
                  -- Some PL Eval Parse error. Output, ignore context change then loop.
                  Left err
                    -> (outputStrLn . Text.unpack . renderDocument $ err) >> loop

                  -- Successful PL Eval Parse. Output, put updated context then
                  -- loop.
                  Right outputTxt
                    -> do outputStrLn . Text.unpack $ outputTxt
                          lift $ put replCtx1
                          liftIO $ writeBChan evChan $ ReplaceReplCtx replCtx1
                          loop

    promptInput = getInputLine "> "

-- | Haskeline settings such as history and auto-completion.
getHaskelineSettings
  :: ( MonadState (ReplCtx Var TyVar) m
     , MonadIO m
     )
  => IO (Settings m)
getHaskelineSettings = do
  hf <- getAppUserDataDirectory "pl.history"
  pure Settings
    { historyFile    = Just hf
    , complete       = completer
    , autoAddHistory = True
    }
  where
    completer = completeWord Nothing [' ', '\t'] $ \w -> do
      s <- get
      return . map simpleCompletion . filter (isPrefixOf w) $ []

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \run ->
                    fmap (flip runStateT s) $ f $ stateRunIO s run
      where
        stateRunIO :: s -> RunIO m -> RunIO (StateT s m)
        stateRunIO s (RunIO run) = RunIO (\m -> fmap (StateT . const)
                                        $ run (runStateT m s))

