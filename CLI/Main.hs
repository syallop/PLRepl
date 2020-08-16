{-# LANGUAGE
    FlexibleContexts
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
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
import PL.FixPhase
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
import PLParser
import Reversible
import Reversible.Iso
import PLLispy
import qualified PLLispy.Name as Lispy

-- External
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Options.Applicative hiding (Parser, ParserInfo)
import Options.Applicative.Types
import System.Exit
import System.IO
import System.Posix.IO
import System.Posix.Terminal
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

  -- | Lookup an expression by it's hash
  | LookupExpr ShortHash

  -- | Lookup a type by it's hash
  | LookupType ShortHash

  -- | Lookup a kind by it's hash
  | LookupKind ShortHash
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
    commandParser = hsubparser . mconcat . fmap mkCommand $ [terminalRepl, version, lookup]

    terminalRepl = ("repl"   , "start a repl to Read, Evaluate, Print (Loop) code", pure TerminalREPL)
    version      = ("version", "show the version of PL being used",                 pure ShowVersion)
    lookup       = ("lookup",  "lookup something associated with a hash",           hsubparser . mconcat . fmap mkCommand $
                                                                                      [ ("expr", "lookup an expression associated with a hash", lookupExpr)
                                                                                      , ("type", "lookup a type associated with a hash"       , lookupType)
                                                                                      , ("kind", "lookup a kind associated with a hash"       , lookupKind)
                                                                                      ])

    lookupExpr = LookupExpr <$> (argument readShortHash $ mconcat [help "Expression hash", metavar "EXPR_HASH"])
    lookupType = LookupType <$> (argument readShortHash $ mconcat [help "Type hash", metavar "TYPE_HASH"])
    lookupKind = LookupKind <$> (argument readShortHash $ mconcat [help "Kind hash", metavar "KIND_HASH"])

    readShortHash :: ReadM ShortHash
    readShortHash = readGrammar Lispy.shortHash

    -- Use a Grammar to read an argument type
    readGrammar :: Grammar a -> ReadM a
    readGrammar g = do
      let plPrinter = fromMaybe mempty . pprint (toPrinter g)
          plParser  = toParser g

      txt <- Text.pack <$> readerAsk

      case PLParser.runParser plParser txt of
                f@(PLParser.ParseFailure expected cursor)
                  -> readerError . Text.unpack . render . ppParseResult plPrinter $ f

                s@(PLParser.ParseSuccess expr cursor)
                  | noTrailingCharacters $ PLParser.remainder cursor
                   -> pure expr

                  | otherwise
                   -> readerError . Text.unpack . render . mconcat $ [ text "Parse succeeded but there were trailing characters: ", lineBreak, document cursor]

      where
        noTrailingCharacters :: Text -> Bool
        noTrailingCharacters txt = Text.null txt || Text.all (`elem` [' ','\t','\n','\r']) txt


    -- Build a replctx by hijacking the codestore used in the TUI.
    -- TODO: TUI should accept a codestore as an argument/ this logic belongs in
    -- Lib
    replCtx :: ReplCtx
    replCtx = mkReplCtx typeCtx TUI.codeStore

    typeCtx :: TypeCtx
    typeCtx = sharedTypeCtx


runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ShowVersion
    -> runShowVersion

  TerminalREPL
    -> runTerminalREPL

  LookupExpr shortHash
    -> runLookupExpr shortHash replCtx

  LookupType shortHash
    -> runLookupType shortHash replCtx

  LookupKind shortHash
    -> runLookupKind shortHash replCtx
  where
    -- Build a replctx by hijacking the codestore used in the TUI.
    -- TODO: TUI should accept a codestore as an argument/ this logic belongs in
    -- Lib
    replCtx :: ReplCtx
    replCtx = mkReplCtx typeCtx TUI.codeStore

    typeCtx :: TypeCtx
    typeCtx = sharedTypeCtx

runShowVersion :: IO ()
runShowVersion = putStrLn version

runTerminalREPL :: IO ()
runTerminalREPL = TUI.run

runLookupExpr :: ShortHash -> ReplCtx -> IO ()
runLookupExpr = runLookupFor "expr" replResolveExprHash replLookupExpr Lispy.ppExpr

runLookupType :: ShortHash -> ReplCtx -> IO ()
runLookupType = runLookupFor "type" replResolveTypeHash replLookupType Lispy.ppType

runLookupKind :: ShortHash -> ReplCtx -> IO ()
runLookupKind = runLookupFor "kind" replResolveKindHash replLookupKind Lispy.ppKind

-- For a named thing:
-- - Resolve a short hash into a full hash
-- - Lookup the thing associated with the full hash
runLookupFor :: forall a. Text -> (ShortHash -> Repl Hash) -> (Hash -> Repl (Maybe a)) -> (a -> Doc) -> ShortHash -> ReplCtx -> IO ()
runLookupFor thing resolveF lookupF ppF shortHash replCtx = do
  (_replCtx, log, eRes) <- runRepl replCtx $ do
    hash <- resolveF shortHash
    replLog . mconcat $ [ text "Resolved full ", text thing, text " hash: "
                        , lineBreak
                        , indent1 $ string . show $ hash
                        , lineBreak
                        ]
    lookupF hash

  writeDoc log
  case eRes of
    Left err
      -> writeFatalError err

    Right mKind
      -> case mKind of
           Nothing
             -> writeDoc (text $ "There is no known "<>thing<>" with the given hash")

           Just kind
             -> writeDoc . ppF $ kind

{- Internal helper functions -}

-- Write a document to stdout if and only if stdout is connected to an interactive tty, otherwise write nothing.
writeDoc :: Doc -> IO ()
writeDoc doc = do
  stdoutInteractive <- queryTerminal stdOutput
  when stdoutInteractive $ Text.putStrLn . PLPrinter.render $ doc

-- Write an Error to stderr using lispy syntax then fail with a non-zero exit
-- code.
writeFatalError :: Error -> IO ()
writeFatalError err = do
  Text.hPutStrLn stderr . PLPrinter.render . mconcat $ [ppError Lispy.ppDefaultError err, lineBreak]
  exitFailure

-- Assume hashes are provided in Lispy syntax
parseShortHash :: String -> Repl ShortHash
parseShortHash shortHashString = plGrammarParser Lispy.shortHash $ Text.pack shortHashString

