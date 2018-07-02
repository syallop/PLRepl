{-# LANGUAGE
    RankNTypes
  , GADTs
  , ScopedTypeVariables
  , FlexibleContexts
  , OverloadedStrings
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

import PL.Expr
import PLLispy
import PLGrammar
import PLPrinter
import PLRepl.Repl
import qualified PL.Megaparsec as PLMega
import qualified PLParser as PLParser
import PL.Binds
import PL.Type
import PL.Kind
import PL.Abstracts
import PL.Error

import Data.Text (Text)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as Text

import qualified Text.Megaparsec as Mega

-- | A ReplConfig for entire lispy expressions parameterised over individual
-- Grammars for bindings, abstractions and type bindings and accepting a custom Read
-- function to transform Grammars into parsers.
--
-- 'plGrammarParser' and 'megaparsecGrammarParser' are exported and can be used
-- here.
lispyExprReplConfig
  :: ( Show b
     , Show abs
     , Show tb
     , Ord tb
     , Eq b
     , Eq abs
     , Binds b (Type tb)
     , Binds tb Kind
     , Abstracts abs tb
     , Document b
     , Document abs
     , Document tb
     )
  => (forall o. Document o => Grammar o -> Text -> Repl b abs tb o o)
  -> Grammar b
  -> Grammar abs
  -> Grammar tb
  -> ReplConfig b abs tb (Expr b abs tb)
lispyExprReplConfig grammarParser b abs tb = ReplConfig
  { _someGrammar = expr b abs tb     -- The expr grammar with supplied sub-grammars.
  , _read        = grammarParser     -- Supplied read function
  , _eval        = replEvalSimple    -- Use default eval function
  , _print       = printerF b abs tb -- aand a printer we define on the supplied sub-grammars.
  }

lispyTypeReplConfig
  :: ( Show b
     , Show abs
     , Show tb
     , Ord tb
     , Eq b
     , Eq abs
     , Binds b (Type tb)
     , Binds tb Kind
     , Abstracts abs tb
     , Document b
     , Document abs
     , Document tb
     )
  => (forall o. Document o => Grammar o -> Text -> Repl b abs tb o o)
  -> Grammar tb
  -> ReplConfig b abs tb (Type tb)
lispyTypeReplConfig grammarParser tb = ReplConfig
  { _someGrammar = typ tb
  , _read        = grammarParser
  , _eval        = \_ -> pure Nothing -- Parsing Types doesnt define new
                                      -- expressions. We currently dont support defining new types.
  , _print       = \_inputTxt parsedTy Nothing -> pure . mconcat $
                     [ text "parsed type:"
                     , lineBreak
                     , fromMaybe mempty $ pprint (toPrinter (typ tb)) parsedTy
                     ]
  }

-- | Read a Grammar with some Parser, report errors but otherwise do nothing.
readOnlyConfig
  :: Grammar o
  -> (forall o. Document o => Grammar o -> Text -> Repl b abs tb o o)
  -> ReplConfig b abs tb o
readOnlyConfig grammar grammarParser = ReplConfig
  { _someGrammar = grammar
  , _read        = grammarParser
  , _eval        = \_ -> pure Nothing -- Parsing Types doesnt define new
                                      -- expressions. We currently dont support defining new types.
  , _print       = \inputTxt parsedTy Nothing -> pure mempty
  }

-- Convert a Grammar to a parser using PLParser.
plGrammarParser
  :: Document o
  => Grammar o
  -> Text
  -> Repl b abs tb o o
plGrammarParser grammar =
  let plParser = toParser grammar
   in \txt -> case PLParser.runParser plParser txt of
                f@(PLParser.ParseFailure expected cursor)
                  -> replError . EMsg . document $ f

                s@(PLParser.ParseSuccess expr cursor)
                  | Text.null $ PLParser.remainder cursor
                   -> pure expr

                  | otherwise
                   -> replError $ EMsg $ text "Parse succeeded but there were trailing characters: " <> document cursor

-- Convert a Grammar to a parser using Megaparsec.
megaparsecGrammarParser
  :: Grammar o
  -> Text
  -> Repl b abs tb o o
megaparsecGrammarParser grammar =
  let megaparsecParser = PLMega.toParser grammar
   in \txt -> case Mega.runParser megaparsecParser "" txt of
                Left err
                  -> replError . EMsg . document $ err

                Right expr
                  -> pure expr

instance (Ord t, Mega.ShowToken t, Mega.ShowErrorComponent e) => Document (Mega.ParseError t e) where
  document = text . Text.pack . Mega.parseErrorPretty

-- Transform a parsed expr, its reduction and type into some output to print
printerF
  :: ( Show b
     , Show abs
     , Show tb
     , Ord tb
     , Eq b
     , Eq abs
     , Binds b (Type tb)
     , Binds tb Kind
     , Abstracts abs tb
     , Document b
     , Document abs
     , Document tb
     )
  => Grammar b
  -> Grammar abs
  -> Grammar tb
  -> Print b abs tb (Expr b abs tb)
printerF b abs tb inputTxt parsed mEval =
  let -- A printer for expressions.
      exprPrinter = toPrinter $ expr b abs tb

      -- A printer for types.
      typePrinter = toPrinter $ typ tb

   in pure . mconcat $
        [ text "read text:"
        , lineBreak
        , text inputTxt -- TODO: Raw text?
        , lineBreak, lineBreak

        , text "parsed input:"
        , lineBreak
        , indent 1 $ fromMaybe mempty $ pprint exprPrinter parsed
        , lineBreak, lineBreak
        ]
        ++
        case mEval of
          Nothing
            -> []

          Just (redExpr, ty)
            -> [ text "which reduces to: "
               , lineBreak
               , indent 1 $ fromMaybe mempty $ pprint exprPrinter redExpr
               , lineBreak, lineBreak

               , text "with type:"
               , lineBreak
               , indent 1 $ fromMaybe mempty $ pprint typePrinter ty
               , lineBreak
               ]

