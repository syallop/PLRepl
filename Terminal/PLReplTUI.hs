{-# LANGUAGE
    FlexibleContexts
  , ImplicitParams
  , OverloadedStrings
  , TemplateHaskell
  , TypeSynonymInstances
  , FlexibleInstances
  #-}
{-|
Module      : PLReplTUI
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
module PLReplTUI
  ( run
  , codeStore
  )
  where

import PLRepl.Repl          as PL
import PLRepl.Repl.Lispy    as PL
import PLRepl.Widgets.Event as PL
import PLRepl.Widgets.Name  as PL
import PLRepl.Widgets.State as PL

import PLLispy
import PLLispy.Expr
import PLLispy.Level
import PL.Hash
import PL.HashStore
import PL.CodeStore
import PL.TyVar
import PL.Expr
import PL.Commented
import PL.Type hiding (void)
import PL.Var
import PL.HashStore
import PL.Serialize
import PL.Store
import PL.Store.Nested
import PL.Store.File
import PL.Store.Memory
import PL.Error
import PL.TypeCtx
import PL.Kind
import PL.Name

import PL.Test.Source
import qualified PL.Test.Expr as Test
import qualified PL.Test.ExprTestCase as Test
import qualified PLLispy.Test.Sources.Expr as Test

import PLGrammar
import Reversible.Iso
import Reversible

import qualified PLParser as PLParser
import qualified PLPrinter as PLPrinter
import qualified PLGrammar as G
import qualified PLLispy as L
import qualified PLLispy.Level as L
import PLPrinter
import PLPrinter.Doc

import qualified PLEditor as E

import Brick hiding (App)
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import qualified Graphics.Vty as Vty
import qualified Brick as Brick

import Control.Concurrent (threadDelay, forkIO, forkFinally)
import Control.Arrow (first)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import System.Directory
import System.Exit
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import Data.Maybe

-- | The ReplApp is a Brick App which handles `Events` to update `State`
-- making use of `Name`'s to name resources.
type ReplApp = Brick.App (PL.State PL.Name) (PL.Event PL.Name) PL.Name

-- | replApp is a Brick app providing a repl for PL.
replApp
  :: BChan (PL.Event PL.Name)
  -> ReplApp
replApp chan = Brick.App
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
handleEvent chan st ev = case ev of
  -- an event from our application
  AppEvent appEv -> case appEv of
    -- Repl should be replaced
    PL.ReplaceCurrentRepl replacementRepl
      -> continue st{_currentRepl = replacementRepl}

    -- An event to the editor
    PL.EditorEv editorEv
      -> continue st{_editorState = handleEditorEventDefault editorEv $ _editorState st}

    PL.OutputEv outputEv
      -> continue st{_outputState = handleOutputEventDefault outputEv $ _outputState st}

    PL.TypeCtxEv typeCtxEv
      -> continue st{_typeCtxState = handleTypeCtxEventDefault typeCtxEv $ _typeCtxState st}

    PL.UsageEv usageEv
      -> continue st{_usageState = handleUsageEventDefault usageEv $ _usageState st}

    PL.FocusOn n
      -> continue st{_focusOn = n}

  -- A virtual terminal event. Most events will be sent to whatever widget we
  -- consider focused. Some may be global.
  VtyEvent vtyEv -> case vtyEv of
    -- A key with no modifiers
    Vty.EvKey keyEv modifiers -> case keyEv of

      -- up arrow => cursor up
      -- +ctrl    => taller view
      Vty.KUp -> case modifiers of
        []
          -> sendToFocused chan CursorUp (_focusOn st) >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (TallerView 1) (_focusOn st) >> continue st
        _ -> continue st

      -- down arrow => cursor down
      -- +ctrl      => shorter view
      Vty.KDown -> case modifiers of
        []
          -> sendToFocused chan CursorDown (_focusOn st) >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (TallerView (-1)) (_focusOn st) >> continue st
        _ -> continue st

      -- left arrow => cursor left
      -- +ctrl      => narrower view
      Vty.KLeft -> case modifiers of
        []
          -> sendToFocused chan CursorLeft (_focusOn st) >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (WiderView (-1)) (_focusOn st) >> continue st
        _ -> continue st

      -- right arrow => cursor right
      -- +ctrl       => wider view
      Vty.KRight -> case modifiers of
        []
          -> sendToFocused chan CursorRight (_focusOn st) >> continue st
        [Vty.MCtrl]
          -> sendToFocused chan (WiderView 1) (_focusOn st) >> continue st
        _ -> continue st

      -- any character => insert that character in the editor.
      -- TODO: Should this still happen when another widget has focus?
      Vty.KChar c
        -> case modifiers of
             []
               -> liftIO (writeBChan chan . EditorEv . InsertChar $ c) >> continue st
             [Vty.MCtrl]
               | c == 'l'
                -> liftIO (writeBChan chan . EditorEv $ Clear) >> continue st

               | otherwise
                -> continue st

             _ -> continue st

      -- delete => delete a character in the editor.
      -- TODO: Should this still happen when another widget has focus?
      Vty.KDel
        -> do liftIO (writeBChan chan . EditorEv $ CursorRight)
              liftIO (writeBChan chan . EditorEv $ DeleteChar)
              continue st
      Vty.KBS
        -> liftIO (writeBChan chan . EditorEv $ DeleteChar) >> continue st

      -- home => Replace the contents of the editor with a random code fragment
      Vty.KHome
        -> do randomCode <- liftIO $ randomExample
              liftIO (writeBChan chan . EditorEv $ Clear)
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
        -> do let txt = editorText $ _editorState $ st
              (log,eRes) <- liftIO . (`step` txt) . _currentRepl $ st
              case eRes of
                -- Some repl error
                Left err
                  -- TODO:
                  -- - Printer should be able to render a document to a list
                  --   of lines with and without a DocFmt to prevent
                  --   re-detecting the newlines.
                  -- - The printer should be passed the current width so it
                  --   wraps optimally.
                  -> continue st{ _outputState  = newOutputState . Text.lines
                                                                 . PLPrinter.render
                                                                 . mconcat $
                                                                     [ lineBreak
                                                                     , log
                                                                     , lineBreak
                                                                     , ppError ppDefaultError $ err
                                                                     ]
                                , _focusOn      = Just OutputCursor
                                }

                -- A successful parse
                Right replacementRepl
                  -> do liftIO (writeBChan chan . ReplaceCurrentRepl $ replacementRepl)
                        continue st{ _currentRepl  = replacementRepl
                                   , _editorState  = emptyEditorState
                                   , _outputState  = newOutputState . Text.lines . renderDocument $ log
                                   , _typeCtxState = typeCtxStateGivenReplTypeCtx . _replTypeCtx . simpleReplCtx $ replacementRepl
                                   , _focusOn      = Just EditorCursor
                                   }

      -- page-up => switch focus to the next widget.
      Vty.KPageUp
        -> liftIO (writeBChan chan . FocusOn . fmap nextFocus . _focusOn $ st) >> continue st

      -- page-down => switch focus to the previous widget.
      Vty.KPageDown
        -> liftIO (writeBChan chan . FocusOn . fmap previousFocus . _focusOn $ st) >> continue st

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
attributes = attrMap Vty.defAttr []

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
    types = borderWithLabel (str "Type context")
          $ padRight Max
          $ padBottom Max
          $ drawTypeCtx TypeCtxCursor (_typeCtxState st)

    usage :: Widget PL.Name
    usage = borderWithLabel (str "Usage")
          $ padRight Max
          $ padBottom Max
          $ drawUsage UsageCursor (_usageState st)

    -- | Draw editor state as a widget with a cursor 'n'.
    -- Text that overflows the line will be cut off rather than wrapping.
    -- TODO: Extend the widget to scroll the editors view.
    drawEditor
      :: n
      -> EditorState
      -> Widget n
    drawEditor editorCursor (EditorState editor view) =
      (\(lines,pos) -> Brick.showCursor editorCursor (Location pos) . txt
                                                                    . Text.unlines
                                                                    . map E.lineText
                                                                    . E.renderLines
                                                                    $ lines
      ) . E.viewEditor view
        $ editor

    drawOutput
      :: n
      -> OutputState
      -> Widget n
    drawOutput = drawEditor

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

    -- Text that overflows the line will be wrapped by Brick. This means:
    -- - The containing widget must limit its horizontal length
    -- - The editors viewport remains the limit on how long a line will be wrapped
    -- before giving up.
    -- - TODO: Add wrapping logic to the Editor
    drawUsage
      :: n
      -> UsageState
      -> Widget n
    drawUsage usageCursor (EditorState editor view) =
      (\(lines,pos) -> Brick.showCursor usageCursor (Location pos) . txtWrap
                                                                   . Text.unlines
                                                                   . map E.lineText
                                                                   . E.renderLines
                                                                   $ lines
      ) . E.viewEditor view
        $ editor

-- Backing storage is a memory cache over the filesystem. We use this to back
-- a HashStore for expressions, allowing them to be persisted across
-- invocations of the REPL.
--
-- We use an orphan instance to serialize Exprs meaning the filestore cannot
-- easily be shared. We guard against this by nesting under a '/lispy'
-- subdirectory.
--
-- Ideally a canonical serialization format would exist.
codeStore :: CodeStore
codeStore = newCodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
  where
    exprStore = newNestedStore newEmptyMemoryStore exprFileStore
    typeStore = newNestedStore newEmptyMemoryStore typeFileStore
    kindStore = newNestedStore newEmptyMemoryStore kindFileStore

    exprTypeStore = newNestedStore newEmptyMemoryStore exprTypeFileStore
    typeKindStore = newNestedStore newEmptyMemoryStore typeKindFileStore

    hashGrammar :: Grammar Hash
    hashGrammar = hashIso \$/ (alg \* charIs '/')
                          \*/ hashTextBrokenAt32
      where
        hashIso :: Iso (HashAlgorithm, Text) Hash
        hashIso = Iso
          {_forwards  = \(alg,bytes) -> either (const Nothing) Just . mkBase58 alg $ bytes
          ,_backwards = Just . unBase58
          }

        alg :: Grammar HashAlgorithm
        alg = sha512

        sha512 :: Grammar HashAlgorithm
        sha512 = (textIs "SHA512" \|/ textIs "sha512") */ rpure SHA512

        hashTextBrokenAt32 :: Grammar Text
        hashTextBrokenAt32 = iso \$/ (takeNWhen 32 isHashCharacter)
                                 \*/ (charIs '/' */ longestMatching isHashCharacter)
          where
            iso :: Iso (Text,Text) Text
            iso = Iso
              {_forwards  = \(prefix,suffix) -> Just $ prefix <> suffix
              ,_backwards = Just . Text.splitAt 32
              }

        takeNWhen :: Int -> (Char -> Bool) -> Grammar Text
        takeNWhen 0 _    = rpure ""
        takeNWhen n pred = consIso \$/ charWhen pred \*/ takeNWhen (n-1) pred

        consIso :: Iso (Char,Text) Text
        consIso = Iso
          { _forwards = \(c,t) -> Just . Text.cons c $ t
          , _backwards = \t -> Text.uncons t
          }

        hashCharacter :: Grammar Char
        hashCharacter = charWhen isHashCharacter

        isHashCharacter :: Char -> Bool
        isHashCharacter = (`elem` hashCharacters)

        -- Base58
        hashCharacters :: [Char]
        hashCharacters = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

    mkFileStore :: (Eq a,Ord a,Serialize a) => Text -> Text -> FileStore Hash a
    mkFileStore subdir file = FileStore
      { _subDirectories =
          [".pl"
          ,"lispy"
          , encodeUtf8 subdir
          ]
      , _filePattern          = hashGrammar \* (charIs '/' */ textIs file)
      , _serializeFileBytes   = serialize
      , _deserializeFileBytes = deserialize
      , _valuesEqual          = (==)
      }

    exprFileStore :: FileStore Hash Expr
    exprFileStore = mkFileStore "expr" "reduced"

    typeFileStore :: FileStore Hash Type
    typeFileStore = mkFileStore "type" "reduced"

    kindFileStore :: FileStore Hash Kind
    kindFileStore = mkFileStore "kind" "reduced"

    exprTypeFileStore :: FileStore Hash Hash
    exprTypeFileStore = mkFileStore "expr" "type"

    typeKindFileStore :: FileStore Hash Hash
    typeKindFileStore = mkFileStore "type" "kind"

run :: IO ()
run = do
  -- Buffer events
  -- Hitting the buffer causes a STM lock up.
  -- TODO:
  -- - Investigate this
  -- - In particular the buffer is only this high to allow:
  --   - Bursts of scroll events
  --   - Pasting small lines. Ideally paste events would be handled as one event
  --     rather than as an event to insert each individual character.
  evChan <- newBChan 1024
  void $ customMain (Vty.mkVty Vty.defaultConfig) (Just evChan) (replApp evChan) (initialState (Just EditorCursor) usage codeStore)

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
  , "Random example : HOME"
  , "Focus pane     : Pg Up/Down"
  , ""
  , "CTRL+l         : Clear editor"
  , ""
  , "Exit           : ESC"
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
exampleLispyTestCases = Test.mkTestCases Test.sources

