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
import PL.Pattern
import PL.Commented
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
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
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

  -- | Resolve an expression short hash to a hash
  | ResolveExpr ShortHash
  -- | Resolve a type short hash to a hash
  | ResolveType ShortHash
  -- | Resolve a kind short hash to a hash
  | ResolveKind ShortHash

  -- | Lookup an expression by it's hash
  | LookupExpr ShortHash
  -- | Lookup a type by it's hash
  | LookupType ShortHash
  -- | Lookup a kind by it's hash
  | LookupKind ShortHash

  -- | Shorten an expr hash to an unambiguous shorthash
  | ShortenExpr Hash
  -- | Shorten a type hash to an unambiguous shorthash
  | ShortenType Hash
  -- | Shorten a kind hash to an unambiguous shorthash
  | ShortenKind Hash

  -- The Parse* commands are weird because they take the parsed thing as an
  -- argument. This is because the string->* logic is pushed into the command
  -- line argument parser itself.
  -- Another distinction is that the input may include comments whereas the
  -- output might have them stripped.

  -- | Parse a textual representation of an expression, checking it is
  -- syntactically valid only.
  | ParseExpr (ExprFor CommentedPhase)
  -- | Parse a textual representation of a type, checking it is
  -- syntactically valid only.
  | ParseType (TypeFor CommentedPhase)
  -- | Parse a textual representation of a pattern, checking it is
  -- syntactically valid only.
  | ParsePattern (PatternFor CommentedPhase)
 -- | Parse a textual representation of a kind, checking it is
  -- syntactically valid only.
  | ParseKind Kind

  -- | Type check an expression (which involves first resolving the types of any
  -- referenced content bindings.
  | TypeCheck Expr
  deriving Show

-- | Read and parse command line options into a Command.
parseCommand :: IO Command
parseCommand = customExecParser (prefs showHelpOnError) commandParserInfo
  where
    commandParserInfo :: O.ParserInfo Command
    commandParserInfo = info (commandParser <**> helper) (progDescDoc $ Just usage)

    usage = ANSI.string . Text.unpack . render . mconcat $
      [ text "pl is an entry point for interaction with the pl language and it's surrounding capabilities."
      , lineBreak
      , lineBreak
      , text "Language overview:\thttps://github.com/syallop/PL"
      , lineBreak
      , text "Hosted repl:\t\thttps://yallop.computer/PL"
      , lineBreak
      , lineBreak

      , text "Examples:"
      , lineBreak

      , indent 1 . mconcat $
          [ text "Open a repl for entering, storing and evaluating expressions"
          , lineBreak
          , indent1 . mconcat $
              [ text "> pl repl"
              , lineBreak
              ]
          , lineBreak

          , text "Parse an expression"
          , lineBreak
          , indent1 . mconcat $
              [ text "> pl parse expr '\\(*) 0'"
              , lineBreak
              , text "λ(*) 0"
              , lineBreak
              ]
          , lineBreak

          , text "Lookup an expression"
          , lineBreak
          , indent1 . mconcat $
              [ text "> pl lookup expr \"#B3rh4i\""
              , lineBreak
              , text "Resolved full expr hash:"
              , lineBreak
              , text " SHA512/B3rh4iGQFySGHtFudSDBeCoxMmvhMPQxnG3fo6Xv9B23vdNUjVvX9u6GgchgGEr3LoTmJZdjXycj3PRzUepEy6RfbyAvQH6FZ52fkyV6uaj9AH6StTY15an3uLZz9mKzRFkgVPo1vJkBmyRZLR4PJRzPttGZvjpkbAnHBZz8iw5kKYo"
              , lineBreak
              , text "+0(*) (*) (μ(KIND) (+(*) %))"
              , lineBreak
              ]
          , lineBreak

          , text "Resolve a short hash"
          , lineBreak
          , indent1 . mconcat $
              [ text "> pl resolve expr \"#B3rh4i\""
              , lineBreak
              , text "#SHA512/B3rh4iGQFySGHtFudSDBeCoxMmvhMPQxnG3fo6Xv9B23vdNUjVvX9u6GgchgGEr3LoTmJZdjXycj3PRzUepEy6RfbyAvQH6FZ52fkyV6uaj9AH6StTY15an3uLZz9mKzRFkgVPo1vJkBmyRZLR4PJRzPttGZvjpkbAnHBZz8iw5kKYo"
              , lineBreak
              ]
          , lineBreak

          , text "Shorten a long hash"
          , lineBreak
          , indent1 . mconcat $
              [ text "> pl shorten expr \"#SHA512/B3rh4iGQFySGHtFudSDBeCoxMmvhMPQxnG3fo6Xv9B23vdNUjVvX9u6GgchgGEr3LoTmJZdjXycj3PRzUepEy6RfbyAvQH6FZ52fkyV6uaj9AH6StTY15an3uLZz9mKzRFkgVPo1vJkBmyRZLR4PJRzPttGZvjpkbAnHBZz8iw5kKYo\""
              , lineBreak
              , text "#B3r"
              , lineBreak
              ]
          , lineBreak
         ]
      ]

    mkCommand (name, desc, parser) = command name $ info parser (progDesc desc)

    -- Top-level commands
    commandParser :: O.Parser Command
    commandParser = hsubparser . mconcat . fmap mkCommand $
      [ terminalRepl
      , version

      , lookup
      , resolve
      , shorten

      , parse
      , typeCheck
      ]

    terminalRepl = ("repl"   , "start a repl to Read, Evaluate, Print (Loop) code", pure TerminalREPL)
    version      = ("version", "show the version of PL being used",                 pure ShowVersion)

    -- Sub-commands of lookup
    lookup = ("lookup", "lookup something associated with a hash", hsubparser . mconcat . fmap mkCommand $
      [ ("expr", "lookup an expression associated with a hash", lookupExpr)
      , ("type", "lookup a type associated with a hash"       , lookupType)
      , ("kind", "lookup a kind associated with a hash"       , lookupKind)
      ])
     where
      lookupExpr = LookupExpr <$> (argument readShortHash $ mconcat [help "Expression hash", metavar "EXPR_HASH"])
      lookupType = LookupType <$> (argument readShortHash $ mconcat [help "Type hash", metavar "TYPE_HASH"])
      lookupKind = LookupKind <$> (argument readShortHash $ mconcat [help "Kind hash", metavar "KIND_HASH"])

    -- Sub-commands of resolve
    resolve = ("resolve", "resolve a short hash to an unambiguous long-hash", hsubparser . mconcat . fmap mkCommand $
      [ ("expr", "resolve an expressions short hash to an unambiguous long-hash", resolveExpr)
      , ("type", "resolve a types short hash to an unambiguous long-hash"       , resolveType)
      , ("kind", "resolve a kinds short hash to an unambiguous long-hash"       , resolveKind)
      ])
     where
      resolveExpr = ResolveExpr <$> (argument readShortHash $ mconcat [help "Expression hash", metavar "EXPR_HASH"])
      resolveType = ResolveType <$> (argument readShortHash $ mconcat [help "Type hash", metavar "TYPE_HASH"])
      resolveKind = ResolveKind <$> (argument readShortHash $ mconcat [help "Kind hash", metavar "KIND_HASH"])

    -- Sub-commands of shorten
    shorten = ("shorten", "shorten a hash to a (currently) unambiguous short-hash", hsubparser . mconcat . fmap mkCommand $
      [ ("expr", "shorten an expressions hash to a (currently) unambiguous short-hash", shortenExpr)
      , ("type", "shorten a types hash to a (currently) unambiguous short-hash"       , shortenType)
      , ("kind", "shorten a kinds hash to a (currently) unambiguous short-hash"       , shortenKind)
      ])
     where
      shortenExpr = ShortenExpr <$> (argument readHash $ mconcat [help "Expression hash", metavar "EXPR_HASH"])
      shortenType = ShortenType <$> (argument readHash $ mconcat [help "Type hash", metavar "TYPE_HASH"])
      shortenKind = ShortenKind <$> (argument readHash $ mconcat [help "Kind hash", metavar "KIND_HASH"])

    -- Sub-commands of parse
    parse = ("parse", "parse code, checking that it is syntactically correct only", hsubparser . mconcat . fmap mkCommand $
      [ ("expr", "parse an expression, checking that it is syntactically correct only", parseExpr)
      , ("type", "parse a type, checking that it is syntactically correct only"       , parseType)
      , ("pattern", "parse a pattern, checking that it is syntactically correct only" , parsePattern)
      , ("kind", "parse a kind, checking that it is syntactically correct only"       , parseKind)
      ])

     where
      parseExpr    = ParseExpr    <$> (argument readCommentedExpr    $ mconcat [help "Expression text", metavar "EXPR_TEXT"])
      parseType    = ParseType    <$> (argument readCommentedType    $ mconcat [help "Type text", metavar "TYPE_TEXT"])
      parsePattern = ParsePattern <$> (argument readCommentedPattern $ mconcat [help "Pattern text", metavar "PATTERN_TEXT"])
      parseKind    = ParseKind    <$> (argument readKind    $ mconcat [help "Kind text", metavar "KIND_TEXT"])

    typeCheck = ("typecheck", "check an expression is well-typed", TypeCheck <$> (argument readExpr $ mconcat [help "Expression text", metavar "EXPR_TEXT"])) 

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

  -- Lookup short/long hashes associated code
  LookupExpr shortHash
    -> runLookupExpr shortHash replCtx

  LookupType shortHash
    -> runLookupType shortHash replCtx

  LookupKind shortHash
    -> runLookupKind shortHash replCtx

  -- Resolve short hashes to long hashes
  ResolveExpr shortHash
    -> runResolveExpr shortHash replCtx

  ResolveType shortHash
    -> runResolveType shortHash replCtx

  ResolveKind shortHash
    -> runResolveKind shortHash replCtx

  -- Shorten long hashes to unambigous short-hashes
  ShortenExpr hash
    -> runShortenExpr hash replCtx

  ShortenType hash
    -> runShortenType hash replCtx

  ShortenKind hash
    -> runShortenKind hash replCtx

  ParseExpr commentedExpr
    -> runParseExpr commentedExpr replCtx

  ParseType commentedType
    -> runParseType commentedType replCtx

  ParsePattern commentedPattern
    -> runParsePattern commentedPattern replCtx

  ParseKind kind
    -> runParseKind kind replCtx

  TypeCheck expr
    -> runTypeCheck expr replCtx
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

runResolveExpr :: ShortHash -> ReplCtx -> IO ()
runResolveExpr = runResolveFor replResolveExprHash

runResolveType :: ShortHash -> ReplCtx -> IO ()
runResolveType = runResolveFor replResolveTypeHash

runResolveKind :: ShortHash -> ReplCtx -> IO ()
runResolveKind = runResolveFor replResolveKindHash

runShortenExpr :: Hash -> ReplCtx -> IO ()
runShortenExpr = runShortenFor replShortenExprHash

runShortenType :: Hash -> ReplCtx -> IO ()
runShortenType = runShortenFor replShortenTypeHash

runShortenKind :: Hash -> ReplCtx -> IO ()
runShortenKind = runShortenFor replShortenKindHash

runParseExpr :: ExprFor CommentedPhase -> ReplCtx -> IO ()
runParseExpr = runParseFor Lispy.ppCommentedExpr

runParseType :: TypeFor CommentedPhase -> ReplCtx -> IO ()
runParseType = runParseFor Lispy.ppCommentedType

runParsePattern :: PatternFor CommentedPhase -> ReplCtx -> IO ()
runParsePattern = runParseFor Lispy.ppCommentedPattern

runParseKind :: Kind -> ReplCtx -> IO ()
runParseKind = runParseFor Lispy.ppKind

runTypeCheck :: Expr -> ReplCtx -> IO ()
runTypeCheck expr replCtx = do
  (_replCtx, log, eType) <- runRepl replCtx $ replResolveAndTypeCheck expr
  writeDoc log
  case eType of
    Left err
      -> writeFatalError err

    Right typ
      -> writeDoc . ppType $ typ

-- For a named thing:
-- - Resolve a short hash into a full hash
-- - Lookup the thing associated with the full hash
runLookupFor :: forall a. Text -> (ShortHash -> Repl Hash) -> (Hash -> Repl (Maybe a)) -> (a -> Doc) -> ShortHash -> ReplCtx -> IO ()
runLookupFor thing resolveF lookupF ppThing shortHash replCtx = do
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

    Right mThing
      -> case mThing of
           Nothing
             -> writeDoc (text $ "There is no known "<>thing<>" with the given hash")

           Just thing
             -> writeDoc . ppThing $ thing

-- For a type of short hash, resolve it into a full hash.
runResolveFor :: (ShortHash -> Repl Hash) -> ShortHash -> ReplCtx -> IO ()
runResolveFor resolveF shortHash replCtx = do
  (_replCtx, log, eRes) <- runRepl replCtx $ resolveF shortHash
  writeDoc log
  case eRes of
    Left err
      -> writeFatalError err

    Right hash
      -> writeDoc . mconcat $ [ fromMaybe mempty . pprint (toPrinter (PLGrammar.charIs '#' */ Lispy.base58Hash)) $ hash
                              , lineBreak
                              ]

-- For a type of long hash, resolve it to the shortest unambiguous hash given
-- the set of known hashes.
runShortenFor :: (Hash -> Repl ShortHash) -> Hash -> ReplCtx -> IO ()
runShortenFor shortenF hash replCtx = do
  (_replCtx, log, eRes) <- runRepl replCtx $ shortenF hash
  writeDoc log
  case eRes of
    Left err
      -> writeFatalError err

    Right shortHash
      -> writeDoc . mconcat $ [ fromMaybe mempty . pprint (toPrinter (Lispy.shortHash)) $ shortHash
                              , lineBreak
                              ]

runParseFor :: (a -> Doc) -> a -> ReplCtx -> IO ()
runParseFor ppThing thing _replCtx = writeDoc . ppThing $ thing


{- Internal helper functions -}

-- Write a document to stdout if and only if stdout is connected to an interactive tty, otherwise write nothing.
writeDoc :: Doc -> IO ()
writeDoc doc = do
  stdoutInteractive <- queryTerminal stdOutput
  when stdoutInteractive $ Text.putStr . PLPrinter.render $ doc

-- Write an Error to stderr using lispy syntax then fail with a non-zero exit
-- code.
writeFatalError :: Error -> IO ()
writeFatalError err = do
  Text.hPutStrLn stderr . PLPrinter.render . mconcat $ [ppError Lispy.ppDefaultError err, lineBreak]
  exitFailure

-- Assume hashes are provided in Lispy syntax
parseShortHash :: String -> Repl ShortHash
parseShortHash shortHashString = plGrammarParser Lispy.shortHash $ Text.pack shortHashString

readShortHash :: ReadM ShortHash
readShortHash = readGrammar Lispy.shortHash

readHash :: ReadM Hash
readHash = readGrammar (PLGrammar.charIs '#' */ Lispy.base58Hash)

readCommentedExpr :: ReadM (ExprFor CommentedPhase)
readCommentedExpr = readGrammar Lispy.commentedExprGrammar

readCommentedType :: ReadM (TypeFor CommentedPhase)
readCommentedType = readGrammar Lispy.commentedTypeGrammar

readCommentedPattern :: ReadM (PatternFor CommentedPhase)
readCommentedPattern = readGrammar Lispy.commentedPatternGrammar

readKind :: ReadM Kind
readKind = readGrammar PLLispy.kind

readExpr :: ReadM Expr
readExpr = readGrammar Lispy.exprGrammar

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

