{-# LANGUAGE
    FlexibleContexts
  , ImplicitParams
  , OverloadedStrings
  , TemplateHaskell
  , TypeSynonymInstances
  , FlexibleInstances
  #-}
{-|
Module      : Main
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

A traditional request-response cli for PL.

Eventually this should be an entry point to all interaction with the language and it's surrounding capabilities.
-}
module Main where

-- Repl
import PLRepl.Repl          as PL
import PLRepl.Repl.Lispy    as PL
import qualified PLReplTUI as TUI

-- Core PL
import PL.CodeStore
import PL.Expr
import PL.Hash
import PL.Kind
import PL.Serialize
import PL.Store
import PL.Store.File
import PL.Store.Memory
import PL.Store.Nested
import PL.Type hiding (void)

-- Other PL
import Reversible
import Reversible.Iso
import PLGrammar

-- External
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)
import qualified Options.Applicative as O
import Options.Applicative hiding (Parser, ParserInfo)

-- Hardcode the version of the CLI.
-- TODO: Consider extracting from .cabal file on build
version :: String
version = "CLI:0.1.0.0 (Language:0.2.0.0)"

main :: IO ()
main = parseCommand >>= runCommand

-- | A Command is an action the CLI should understand how to accept as command
-- line arguments and execute for some result.
data Command
  -- | Print a version string indicating CLI compatibility.
  = ShowVersion

  -- | Execute the TUI repl.
  | TerminalREPL
  deriving Show

-- | Read and parse command line options into a Command.
parseCommand :: IO Command
parseCommand = customExecParser (prefs showHelpOnError) commandParserInfo
  where
    commandParserInfo :: O.ParserInfo Command
    commandParserInfo = info (commandParser <**> helper) (progDescDoc $ Just usage)

    usage = "Usage"

    commandParser :: O.Parser Command
    commandParser = hsubparser (mconcat [terminalRepl, version])

    version = command "version" $ info (pure ShowVersion) (progDesc "show the version of PL being used")

    terminalRepl = command "repl" $ info (pure TerminalREPL) (progDesc "start a repl to Read, Evaluate, Print (Loop) code")

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ShowVersion
    -> putStrLn version

  TerminalREPL
    -> TUI.run

