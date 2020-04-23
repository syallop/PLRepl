{-# LANGUAGE
    RankNTypes
  , GADTs
  , ScopedTypeVariables
  , FlexibleContexts
  , OverloadedStrings
  , UndecidableInstances
  , TypeSynonymInstances
  , FlexibleInstances
  , ImplicitParams
  #-}
{-|
Module      : PLRepl.Repl
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

A PLRepl.Repl for lispy-style grammars.

|-}
module PLRepl.Repl.Lispy
  ( lispyExprReplConfig
  , lispyTypeReplConfig
  , readOnlyConfig
  , plGrammarParser
  , megaparsecGrammarParser
  )
  where

import PL.Binds
import PL.Error
import PL.Expr
import PL.Kind
import PL.TyVar
import PL.Type
import PL.Var
import PLGrammar
import PLLispy
import PLLispy.Level
import PLPrinter
import PLRepl.Repl
import qualified PL.Megaparsec as PLMega
import qualified PLParser as PLParser

import Data.Text (Text)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Map as Map

import qualified Text.Megaparsec as Mega

-- | A ReplConfig for entire lispy expressions parameterised over individual
-- Grammars for bindings, abstractions and type bindings and accepting a custom Read
-- function to transform Grammars into parsers.
--
-- 'plGrammarParser' and 'megaparsecGrammarParser' are exported and can be used
-- here.
lispyExprReplConfig
  :: (Text -> Repl DefaultPhase Expr Expr)
  -> Grammar Var
  -> Grammar Type
  -> Grammar TyVar
  -> ReplConfig DefaultPhase Expr
lispyExprReplConfig grammarParser b abs tb =
  let ?eb  = b
      ?abs = abs
      ?tb  = tb
   in ReplConfig
        { _someGrammar = top $ expr b abs tb
        , _read        = grammarParser
        , _eval        = replEvalSimple
        , _print       = printerF (fromMaybe mempty . pprint (toPrinter $ top $ typ tb))
        }

-- | A ReplConfig for type signatures parameterised over a Grammar for type
-- bindings and accepting a custom Read function to transform Grammars into parsers.
--
-- 'plGrammarParser' and 'megaparsecGrammarParser' are exported and can be used
-- here.
lispyTypeReplConfig
  :: (phase ~ DefaultPhase)
  => (Text -> Repl phase (TypeFor phase) (TypeFor phase))
  -> Grammar (TypeBindingFor phase)
  -> ReplConfig phase (TypeFor phase)
lispyTypeReplConfig grammarParser tb = ReplConfig
  { _someGrammar = sub $ typ tb
  , _read        = grammarParser
  , _eval        = \_ -> pure Nothing -- Parsing Types doesnt define new
                                      -- expressions. We currently dont support defining new types.
  , _print       = \_inputTxt parsedTy Nothing -> pure . mconcat $
                     [ text "parsed type:"
                     , lineBreak
                     , fromMaybe mempty $ pprint (toPrinter $ sub $ typ tb) parsedTy
                     ]
  }

-- | Read a Grammar with some Parser, report errors but otherwise do nothing.
readOnlyConfig
  :: Grammar o
  -> (Text -> Repl phase o o)
  -> ReplConfig phase o
readOnlyConfig grammar grammarParser = ReplConfig
  { _someGrammar = grammar
  , _read        = grammarParser
  , _eval        = \_ -> pure Nothing -- Parsing Types doesnt define new
                                      -- expressions. We currently dont support defining new types.
  , _print       = \inputTxt parsedTy Nothing -> pure mempty
  }

-- Convert a Grammar to a parser using PLParser.
plGrammarParser
  :: Grammar o
  -> Text
  -> Repl phase o o
plGrammarParser grammar =
  let plParser = toParser grammar
      plPrinter = fromMaybe mempty . pprint (toPrinter grammar)
   in \txt -> case PLParser.runParser plParser txt of
                f@(PLParser.ParseFailure expected cursor)
                  -> replError . EMsg . ppParseResult plPrinter $ f

                s@(PLParser.ParseSuccess expr cursor)
                  | noTrailingCharacters $ PLParser.remainder cursor
                   -> pure expr

                  | otherwise
                   -> replError $ EMsg $ text "Parse succeeded but there were trailing characters: " <> lineBreak <> document cursor

  where
    noTrailingCharacters :: Text -> Bool
    noTrailingCharacters txt = Text.null txt || Text.all (`elem` [' ','\t','\n','\r']) txt

ppParseResult
  :: (a -> Doc)
  -> PLParser.ParseResult a
  -> Doc
ppParseResult ppA p = case p of
    PLParser.ParseSuccess a leftovers
      -> text "Parsed: " <> ppA a <> text "with leftovers" <> document leftovers

    PLParser.ParseFailure failures cur0
      -> mconcat $
           [ text "Parse failure at:"
           , lineBreak

           , indent1 $ document cur0
           , lineBreak
           ]
           ++
           if null failures
             then mempty
             else [ text "The failures backtracked from were:"
                  , lineBreak
                  , indent1 . mconcat
                            . map (\(cursor,expected) -> mconcat [ document cursor
                                                                 , document expected
                                                                 , lineBreak
                                                                 , lineBreak
                                                                 ]
                                  )
                            . Map.toList
                            . PLParser.collectFailures
                            $ failures
                  ]


-- Convert a Grammar to a parser using Megaparsec.
megaparsecGrammarParser
  :: Grammar o
  -> Text
  -> Repl phase o o
megaparsecGrammarParser grammar =
  let megaparsecParser = PLMega.toParser grammar
   in \txt -> case Mega.runParser megaparsecParser "" txt of
                Left err
                  -> replError . EMsg . string. Mega.errorBundlePretty $ err

                Right expr
                  -> pure expr

-- Transform a parsed expr, its reduction and type into some output to print
printerF
  :: (TypeFor phase -> Doc)
  -> Print phase (ExprFor phase)
printerF ppType = \inputTxt parsed mEval -> do
  grammar <- replGrammar
  let ppExpr = fromMaybe mempty . pprint (toPrinter grammar)

  pure . mconcat $
    [ text "read text:"
    , lineBreak
    , rawText inputTxt
    , lineBreak, lineBreak

    , text "parsed input:"
    , lineBreak
    , indent 1 $ ppExpr parsed
    , lineBreak, lineBreak
    ]
    ++
    case mEval of
      Nothing
        -> []

      Just (redExpr, ty)
        -> [ text "which reduces to: "
           , lineBreak
           , indent 1 $ ppExpr redExpr
           , lineBreak, lineBreak

           , text "with type:"
           , lineBreak
           , indent 1 $ ppType ty
           , lineBreak
           ]

