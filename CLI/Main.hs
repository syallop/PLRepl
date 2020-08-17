{-# LANGUAGE
    FlexibleContexts
  , ImplicitParams
  , OverloadedStrings
  , GADTs
  , RankNTypes
  , ScopedTypeVariables
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

{- TODO:
- Any complex repl logic here 'belongs' under the shared Lib to allow sharing
  with TUI/ web repl which might want some degree of feature parity.
-}

-- Repl
import PLRepl.Repl         as PL
import PLRepl.Repl.Lispy   as Lispy
import qualified PLReplTUI as TUI

-- Core PL
import PL.Binds
import PL.CodeStore
import qualified PL.Error as PL
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
main = parseCommand >>= runCommand >>= Text.putStr . PLPrinter.render

-- | A Command is an action the CLI should understand how to accept as command
-- line arguments and execute for some result type 'r'.
data Command r where
  -- | Print a version string indicating CLI compatibility.
  ShowVersion :: Command Text

  -- | Execute the TUI repl.
  TerminalREPL :: Command ()

  -- | Resolve an expression short hash to a hash
  ResolveExpr :: ShortHash -> Command Hash
  -- | Resolve a type short hash to a hash
  ResolveType :: ShortHash -> Command Hash
  -- | Resolve a kind short hash to a hash
  ResolveKind :: ShortHash -> Command Hash

  -- | Lookup an expression by it's hash
  LookupExpr :: ShortHash -> Command Expr
  -- | Lookup a type by it's hash
  LookupType :: ShortHash -> Command Type
  -- | Lookup a kind by it's hash
  LookupKind :: ShortHash -> Command Kind

  -- | Shorten an expr hash to an unambiguous shorthash
  ShortenExpr :: Hash -> Command ShortHash
  -- | Shorten a type hash to an unambiguous shorthash
  ShortenType :: Hash -> Command ShortHash
  -- | Shorten a kind hash to an unambiguous shorthash
  ShortenKind :: Hash -> Command ShortHash

  -- The Parse* commands are weird because they take the parsed thing as an
  -- argument. This is because the string->* logic is pushed into the command
  -- line argument parser itself.
  -- Another distinction is that the input may include comments whereas the
  -- output might have them stripped.

  -- | Parse a textual representation of an expression, checking it is
  -- syntactically valid only.
  ParseExpr :: ExprFor CommentedPhase -> Command (ExprFor CommentedPhase)
  -- | Parse a textual representation of a type, checking it is
  -- syntactically valid only.
  ParseType :: TypeFor CommentedPhase -> Command (TypeFor CommentedPhase)
  -- | Parse a textual representation of a pattern, checking it is
  -- syntactically valid only.
  ParsePattern :: PatternFor CommentedPhase -> Command (PatternFor CommentedPhase)
  -- | Parse a textual representation of a kind, checking it is
  -- syntactically valid only.
  ParseKind :: Kind -> Command Kind

  -- | Type check an expression (which involves first resolving the types of any
  -- referenced content bindings.
  TypeCheck :: Expr -> Command Type

instance Show (Command r) where
  show cmd = case cmd of
    ShowVersion
      -> "version"

    TerminalREPL
      -> "repl"

    ResolveExpr _
      -> "resolve expr"

    ResolveType _
      -> "resolve type"

    ResolveKind _
      -> "resolve kind"

    LookupExpr _
      -> "lookup expr"

    LookupType _
      -> "lookup type"

    LookupKind _
      -> "lookup kind"

    ShortenExpr _
      -> "shorten expr"

    ShortenType _
      -> "shorten type"

    ShortenKind _
      -> "shorten kind"

    ParseExpr _
      -> "parse expr"

    ParseType _
      -> "parse type"

    ParsePattern _
      -> "parse pattern"

    ParseKind _
      -> "parse kind"

    TypeCheck _
      -> "typecheck"

-- Forget the specific return type of a command, allowing the fact that it is
-- Printable to be recovered on matches.
data SomeCommand = forall r. Printable r => SomeCommand (Command r)

-- | Read and parse command line options into SomeCommand.
parseCommand :: IO SomeCommand
parseCommand = customExecParser (prefs showHelpOnError) commandParserInfo
  where
    commandParserInfo :: O.ParserInfo SomeCommand
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

    -- Convenience function to construct a command from it's name, description
    -- and parser
    mkCommand :: (String, String, O.Parser SomeCommand) -> Mod CommandFields SomeCommand
    mkCommand (name, desc, parser) = command name $ info parser (progDesc desc)

    -- Top-level commands
    commandParser :: O.Parser SomeCommand
    commandParser = hsubparser . mconcat . fmap mkCommand $
      [ terminalRepl
      , version

      , lookup
      , resolve
      , shorten

      , parse
      , typeCheck
      ]

    terminalRepl = ("repl"   , "start a repl to Read, Evaluate, Print (Loop) code", pure $ SomeCommand TerminalREPL)
    version      = ("version", "show the version of PL being used",                 pure $ SomeCommand ShowVersion)

    -- Sub-commands of lookup
    lookup = ("lookup", "lookup something associated with a hash", hsubparser . mconcat . fmap mkCommand $
      [ ("expr", "lookup an expression associated with a hash", lookupExpr)
      , ("type", "lookup a type associated with a hash"       , lookupType)
      , ("kind", "lookup a kind associated with a hash"       , lookupKind)
      ])
     where
      lookupExpr = SomeCommand . LookupExpr <$> (argument readShortHash $ mconcat [help "Expression hash", metavar "EXPR_HASH"])
      lookupType = SomeCommand . LookupType <$> (argument readShortHash $ mconcat [help "Type hash", metavar "TYPE_HASH"])
      lookupKind = SomeCommand . LookupKind <$> (argument readShortHash $ mconcat [help "Kind hash", metavar "KIND_HASH"])

    -- Sub-commands of resolve
    resolve = ("resolve", "resolve a short hash to an unambiguous long-hash", hsubparser . mconcat . fmap mkCommand $
      [ ("expr", "resolve an expressions short hash to an unambiguous long-hash", resolveExpr)
      , ("type", "resolve a types short hash to an unambiguous long-hash"       , resolveType)
      , ("kind", "resolve a kinds short hash to an unambiguous long-hash"       , resolveKind)
      ])
     where
      resolveExpr = SomeCommand . ResolveExpr <$> (argument readShortHash $ mconcat [help "Expression hash", metavar "EXPR_HASH"])
      resolveType = SomeCommand . ResolveType <$> (argument readShortHash $ mconcat [help "Type hash", metavar "TYPE_HASH"])
      resolveKind = SomeCommand . ResolveKind <$> (argument readShortHash $ mconcat [help "Kind hash", metavar "KIND_HASH"])

    -- Sub-commands of shorten
    shorten = ("shorten", "shorten a hash to a (currently) unambiguous short-hash", hsubparser . mconcat . fmap mkCommand $
      [ ("expr", "shorten an expressions hash to a (currently) unambiguous short-hash", shortenExpr)
      , ("type", "shorten a types hash to a (currently) unambiguous short-hash"       , shortenType)
      , ("kind", "shorten a kinds hash to a (currently) unambiguous short-hash"       , shortenKind)
      ])
     where
      shortenExpr = SomeCommand . ShortenExpr <$> (argument readHash $ mconcat [help "Expression hash", metavar "EXPR_HASH"])
      shortenType = SomeCommand . ShortenType <$> (argument readHash $ mconcat [help "Type hash", metavar "TYPE_HASH"])
      shortenKind = SomeCommand . ShortenKind <$> (argument readHash $ mconcat [help "Kind hash", metavar "KIND_HASH"])

    -- Sub-commands of parse
    parse = ("parse", "parse code, checking that it is syntactically correct only", hsubparser . mconcat . fmap mkCommand $
      [ ("expr", "parse an expression, checking that it is syntactically correct only", parseExpr)
      , ("type", "parse a type, checking that it is syntactically correct only"       , parseType)
      , ("pattern", "parse a pattern, checking that it is syntactically correct only" , parsePattern)
      , ("kind", "parse a kind, checking that it is syntactically correct only"       , parseKind)
      ])
     where
      parseExpr    = SomeCommand . ParseExpr    <$> (argument readCommentedExpr    $ mconcat [help "Expression text", metavar "EXPR_TEXT"])
      parseType    = SomeCommand .ParseType    <$> (argument readCommentedType    $ mconcat [help "Type text", metavar "TYPE_TEXT"])
      parsePattern = SomeCommand .ParsePattern <$> (argument readCommentedPattern $ mconcat [help "Pattern text", metavar "PATTERN_TEXT"])
      parseKind    = SomeCommand .ParseKind    <$> (argument readKind    $ mconcat [help "Kind text", metavar "KIND_TEXT"])

    typeCheck = ("typecheck", "check an expression is well-typed", SomeCommand . TypeCheck <$> (argument readExpr $ mconcat [help "Expression text", metavar "EXPR_TEXT"]))

    -- Build a replctx by hijacking the codestore used in the TUI.
    -- TODO: TUI should accept a codestore as an argument/ this logic belongs in
    -- Lib
    replCtx :: ReplCtx
    replCtx = mkReplCtx typeCtx TUI.codeStore

    typeCtx :: TypeCtx
    typeCtx = sharedTypeCtx

-- Run a command, returning success results as a document to be printed.
-- Failures will write to stderr an exit with a non-0 status code
-- Successful operations may log informative messages to stdout if and only if
-- stdout is an interactive tty.
runCommand :: SomeCommand -> IO Doc
runCommand someCmd = case someCmd of
  SomeCommand cmd
    -> case cmd of
          ShowVersion
            -> pure . ppResult printers . Text.pack $ version

          TerminalREPL
            -> TUI.run >> pure mempty

          -- Lookup short/long hashes associated code
          LookupExpr shortHash
            -> ppResult printers <$> runLookupFor "expr" (replResolveExprHash shortHash) replLookupExpr replCtx

          LookupType shortHash
            -> ppResult printers <$> runLookupFor "type" (replResolveTypeHash shortHash) replLookupType replCtx

          LookupKind shortHash
            -> ppResult printers <$> runLookupFor "kind" (replResolveKindHash shortHash) replLookupKind replCtx

          -- Resolve short hashes to long hashes
          ResolveExpr shortHash
            -> ppResult printers <$> tryRunRepl replCtx (replResolveExprHash shortHash)

          ResolveType shortHash
            -> ppResult printers <$> tryRunRepl replCtx (replResolveTypeHash shortHash)

          ResolveKind shortHash
            -> ppResult printers <$> tryRunRepl replCtx (replResolveKindHash shortHash)

          -- Shorten long hashes to unambigous short-hashes
          ShortenExpr hash
            -> ppResult printers <$> tryRunRepl replCtx (replShortenExprHash hash)

          ShortenType hash
            -> ppResult printers <$> tryRunRepl replCtx (replShortenTypeHash hash)

          ShortenKind hash
            -> ppResult printers <$> tryRunRepl replCtx (replShortenKindHash hash)

          ParseExpr commentedExpr
            -> pure . ppResult printers $ commentedExpr

          ParseType commentedType
            -> pure . ppResult printers $ commentedType

          ParsePattern commentedPattern
            -> pure . ppResult printers $ commentedPattern

          ParseKind kind
            -> pure . ppResult printers $ kind

          TypeCheck expr
            -> ppResult printers <$> tryRunRepl replCtx (replResolveAndTypeCheck expr)

          cmd
            -> writeFatalError . PL.EMsg . mconcat $
                 [ text "pl has parsed a command that it does not understand how to execute:"
                 , lineBreak
                 , string . show $ cmd
                 , lineBreak
                 ]
  where
    -- Build a replctx by hijacking the codestore used in the TUI.
    -- TODO: TUI should accept a codestore as an argument/ this logic belongs in
    -- Lib
    replCtx :: ReplCtx
    replCtx = mkReplCtx typeCtx TUI.codeStore

    typeCtx :: TypeCtx
    typeCtx = sharedTypeCtx

    printers = lispyPrinters



{- Internal helper functions -}

-- For a named thing:
-- - Resolve a short hash into a full hash
-- - Lookup the thing associated with the full hash
runLookupFor :: Text -> Repl Hash -> (Hash -> Repl (Maybe r)) -> ReplCtx -> IO r
runLookupFor thing resolve lookup replCtx = tryRunRepl replCtx $ do
    hash <- resolve
    replLog . mconcat $ [ text "Resolved full ", text thing, text " hash: "
                        , lineBreak
                        , indent1 $ string . show $ hash
                        , lineBreak
                        ]
    mThing <- lookup hash
    case mThing of
      Nothing
        -> replError $ PL.EMsg $ text $ "There is no known "<>thing<>" with the given hash"

      Just thing
        -> pure thing

-- Attempt to run a repl function calling exitFailure if an error is encountered
-- and only writing logs to stdout if it is an interactive tty.
tryRunRepl :: ReplCtx -> Repl a -> IO a
tryRunRepl replCtx repl = do
  (_replCtx, log, eRes) <- runRepl replCtx repl
  writeDoc log
  case eRes of
    Left err
      -> writeFatalError err

    Right res
      -> pure res

lispyPrinters :: Printers
lispyPrinters = Printers
  { _ppExpr = Lispy.ppExpr
  , _ppType = Lispy.ppType
  , _ppKind = Lispy.ppKind
  , _ppCommentedExpr = Lispy.ppCommentedExpr
  , _ppCommentedType = Lispy.ppCommentedType
  , _ppCommentedPattern = Lispy.ppCommentedPattern
  , _ppHash = \hash -> fromMaybe mempty . pprint (toPrinter (PLGrammar.charIs '#' */ Lispy.base58Hash)) $ hash
  , _ppShortHash = \shortHash -> fromMaybe mempty . pprint (toPrinter Lispy.shortHash) $ shortHash
  }

-- A collection of pretty printers for result types of commands.
data Printers = Printers
  { _ppExpr :: Expr -> Doc
  , _ppType :: Type -> Doc
  , _ppKind :: Kind -> Doc
  , _ppCommentedExpr    :: ExprFor CommentedPhase -> Doc
  , _ppCommentedType    :: TypeFor CommentedPhase -> Doc
  , _ppCommentedPattern :: PatternFor CommentedPhase -> Doc
  , _ppHash      :: Hash -> Doc
  , _ppShortHash :: ShortHash -> Doc
  }

-- Class of result types that can be printed to a tty.
--
-- Like Document, except instead of using a single canonical instance the
-- ppResult method takes a record of printers to allow switching out specific
-- syntax used.
class Printable r where ppResult :: Printers -> r -> Doc
instance Printable Text where ppResult _ = text
instance Printable ()   where ppResult _ = mempty
instance Printable Expr where ppResult = _ppExpr
instance Printable Type where ppResult = _ppType
instance Printable Kind where ppResult = _ppKind
instance Printable (ExprFor CommentedPhase) where ppResult = _ppCommentedExpr
instance Printable (TypeFor CommentedPhase) where ppResult = _ppCommentedType
instance Printable (PatternFor CommentedPhase) where ppResult = _ppCommentedPattern
instance Printable Hash where ppResult = _ppHash
instance Printable ShortHash where ppResult = _ppShortHash

-- Write a pretty printed result to stdout
writeResult :: Printable r => Printers -> r -> IO ()
writeResult printers result = Text.putStr . PLPrinter.render . ppResult printers $ result

-- Write a document to stdout if and only if stdout is connected to an interactive tty, otherwise write nothing.
writeDoc :: Doc -> IO ()
writeDoc doc = do
  stdoutInteractive <- queryTerminal stdOutput
  when stdoutInteractive $ Text.putStr . PLPrinter.render $ doc

-- Write an Error to stderr using lispy syntax then fail with a non-zero exit
-- code.
writeFatalError :: Error -> IO x
writeFatalError err = do
  Text.hPutStrLn stderr . PLPrinter.render . mconcat $ [PL.ppError Lispy.ppDefaultError err, lineBreak]
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

