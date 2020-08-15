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
import PLRepl.Repl         as PL
import PLRepl.Repl.Lispy   as Lispy
import qualified PLReplTUI as TUI

-- Core PL
import PL.Binds
import PL.CodeStore
import PL.Error
import PL.Expr
import PL.Hash
import PL.HashStore
import PL.Kind
import PL.Serialize
import PL.Store
import PL.Store.File
import PL.Store.Memory
import PL.Store.Nested
import PL.Test.Shared (sharedTypeCtx)
import PL.Type hiding (void)
import PL.TypeCtx
import PL.Var

-- Other PL
import PLGrammar
import PLPrinter
import Reversible
import Reversible.Iso
import qualified PLLispy.Name as Lispy

-- External
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Options.Applicative hiding (Parser, ParserInfo)
import System.Exit
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Options.Applicative as O


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

  | LookupExpr String
  deriving Show

-- | Read and parse command line options into a Command.
parseCommand :: IO Command
parseCommand = customExecParser (prefs showHelpOnError) commandParserInfo
  where
    commandParserInfo :: O.ParserInfo Command
    commandParserInfo = info (commandParser <**> helper) (progDescDoc $ Just usage)

    usage = "Usage"

    mkCommand (name, desc, parser) = command name $ info parser (progDesc desc)

    commandParser :: O.Parser Command
    commandParser = hsubparser . mconcat . fmap mkCommand $ [terminalRepl, version, lookupExpr]

    terminalRepl = ("repl"   , "start a repl to Read, Evaluate, Print (Loop) code", pure TerminalREPL)
    version      = ("version", "show the version of PL being used",                 pure ShowVersion)
    lookupExpr   = ("lookup",  "lookup the expression associated with a hash",      LookupExpr <$> (strArgument $ mconcat [help "Expression hash", metavar "EXPR_HASH"]))

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ShowVersion
    -> putStrLn version

  TerminalREPL
    -> TUI.run

  LookupExpr shortHashString
    -> do (_replCtx, log, eRes) <- runRepl replCtx $ do
            -- Assume hashes are provided in Lispy syntax
            shortHash <- plGrammarParser Lispy.shortHash $ Text.pack shortHashString
            hash <- replResolveExprHash shortHash
            replLookupExpr hash
          case eRes of
            Left err
              -> do Text.putStrLn . PLPrinter.render . mconcat $
                      [ ppError Lispy.ppDefaultError $ err
                      , lineBreak
                      , log
                      ]
                    exitFailure

            Right mExpr
              -> case mExpr of
                   Nothing
                     -> putStrLn "There are no known expressions with the given hash"

                   Just expr
                     -> Text.putStrLn . PLPrinter.render . Lispy.ppExpr $ expr

  where
    -- Build a replctx by hijacking the codestore used in the TUI.
    -- TODO: TUI should accept a codestore as an argument/ this logic belongs in
    -- Lib
    replCtx :: ReplCtx
    replCtx = mkReplCtx typeCtx TUI.codeStore

    typeCtx :: TypeCtx
    typeCtx = sharedTypeCtx

