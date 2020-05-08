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
import PL.Commented
import PL.Type
import PL.Var
import PLGrammar
import PLLispy
import PLLispy.Level
import PLPrinter
import PLRepl.Repl
import qualified PLParser as PLParser
import PLParser (Cursor,Expected)

import Data.Text (Text)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

import Control.Applicative
import Data.Void
import Reversible
import Reversible.Iso
import qualified PLGrammar as G
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Control.Monad.Combinators.Expr as Mega

-- | A ReplConfig for entire lispy expressions parameterised over individual
-- Grammars for bindings, abstractions and type bindings and accepting a custom Read
-- function to transform Grammars into parsers.
--
-- 'plGrammarParser' and 'megaparsecGrammarParser' are exported and can be used
-- here.
lispyExprReplConfig
  :: (Text -> Repl (ExprFor CommentedPhase) (ExprFor CommentedPhase))
  -> Grammar Var
  -> Grammar (TypeFor CommentedPhase)
  -> Grammar TyVar
  -> ReplConfig (ExprFor CommentedPhase)
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
  :: (Text -> Repl (TypeFor CommentedPhase) (TypeFor CommentedPhase))
  -> Grammar (TypeBindingFor CommentedPhase)
  -> ReplConfig (TypeFor CommentedPhase)
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
  -> (Text -> Repl o o)
  -> ReplConfig o
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
  -> Repl o o
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
     -> let -- Collect expectations that occur at the same cursor position
            collectedFailures :: Map.Map Cursor (Set Expected)
            collectedFailures = collectFailures failures

            -- Ordered list of expectations, deepest first
            failuresByDepth :: [(Cursor,Set Expected)]
            failuresByDepth = Map.toDescList $ Map.delete cur0 $ collectedFailures

            -- Expectations at the exact point the error is reported.
            failureAtStop :: Set Expected
            failureAtStop = fromMaybe Set.empty $ Map.lookup cur0 $ collectedFailures

            -- The deepest set of expectations - this can be later
            -- than the point the error is reported when backtracking
            -- has occured before the failure.
            deepestFailure :: Maybe (Cursor,Set Expected)
            deepestFailure = case failuresByDepth of
              []
                -> Nothing
              [d]
                -> Just d

            -- Failures between the reported error and the deepest
            -- error are places we've backtracked from.
            backtrackedFailures :: [(Cursor,Set Expected)]
            backtrackedFailures = filter (\(c,_) -> c >= cur0) failuresByDepth

            failuresBeforeStop :: [(Cursor,Set Expected)]
            failuresBeforeStop = filter (\(c,_) -> c < cur0) failuresByDepth
        in mconcat $
             [ text "Parse failure at:"
             , lineBreak, lineBreak
             , indent 2 $ document cur0
             , lineBreak
             , if null failures
                 then indent 2 $ text "For no known reason. Which is likely a bug on our part"
                 else mconcat $
                   [ case Set.toList failureAtStop of
                       []
                         -> text "Due to backtracked error"

                       [e]
                         -> text "Where we expected: " <> document e <> lineBreak

                       (e:es)
                         -> mconcat
                              [ text "Where we expected:"
                              , lineBreak
                              , indent 2 $ document $ foldr PLParser.ExpectEither e es
                              ]
                   ]

              , case backtrackedFailures of
                  []
                    -> text "Without having backtracked"

                  (e:es)
                    -> mconcat
                         [ text "Having backtracked due to"
                         , if null es
                             then text " a single failure:"
                             else text " several failures:"
                         , lineBreak
                         , indent 2 $ ppPossibilities (e:es)
                         , lineBreak
                         ]

              , case failuresBeforeStop of
                  []
                    -> mempty
                  (e:es)
                    -> mconcat [ text "It's possible a mistake was made at one of these previous alternatives:"
                               , indent 2 $ ppPossibilities failuresBeforeStop
                               ]
              ]
  where
    ppPossibilities :: [(Cursor, Set Expected)] -> Doc
    ppPossibilities p = indent 2 $ mconcat . map (\(c,es) ->
      case Set.toList es of
        []
          -> mempty
        e:es
          -> mconcat [ document $ c
                     , lineBreak
                     , document $ foldr PLParser.ExpectEither e es
                     ]
      ) $ p


    collectFailures :: [(Expected,Cursor)] -> Map.Map Cursor (Set Expected)
    collectFailures allFailures = foldr (\(expected,cursor) acc
                                          -> Map.insertWith (<>) cursor (Set.singleton expected) acc
                                        )
                                        mempty
                                        allFailures


-- Convert a Grammar to a parser using Megaparsec.
megaparsecGrammarParser
  :: Grammar o
  -> Text
  -> Repl o o
megaparsecGrammarParser grammar =
  let megaparsecParser = toParser grammar
   in \txt -> case Mega.runParser megaparsecParser "" txt of
                Left err
                  -> replError . EMsg . string. Mega.errorBundlePretty $ err

                Right expr
                  -> pure expr
  where
    -- | Convert a Grammar to a Parser that accepts it.
    toParser :: G.Grammar a -> Mega.Parsec Void Text a
    toParser (Reversible r) = case r of
      ReversibleInstr i
        -> case i of
             -- A single character if one is available.
             G.GAnyChar
               -> Mega.anySingle

             -- Enhance a failing parse with a given Expect label.
             G.GLabel l g
               -> toParser g Mega.<?> (Text.unpack $ renderDocument l)

             G.GTry g
               -> Mega.try $ toParser g

      -- Return the value.
      RPure a
        -> pure a

      -- Fail with no Expectations.
      REmpty
        -> empty

      -- If the left fails, try the right as if no input had been consumed.
      RAlt g0 g1
        -> toParser g0 <|> toParser g1

      -- Parse the grammar if the iso succeeds.
      RMap iso ga
        -> rmapParser iso ga

      -- Tuple the result of two successive parsers.
      RAp ga gb
        -> rapParser (toParser ga) (toParser gb)

    rmapParser
      :: Show a
      => Iso a b
      -> G.Grammar a
      -> Mega.Parsec Void Text b
    rmapParser iso g = do
      a <- toParser g
      case forwards iso a of
        Nothing
          -> fail "iso"
        Just b
          -> pure b

    rapParser
      :: Mega.Parsec Void Text a
      -> Mega.Parsec Void Text b
      -> Mega.Parsec Void Text (a, b)
    rapParser pl pr = (,) <$> pl <*> pr

-- Transform a parsed expr, its reduction and type into some output to print
printerF
  :: (TypeFor CommentedPhase -> Doc)
  -> Print (ExprFor CommentedPhase)
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
           , indent 1 . ppExpr $ redExpr
           , lineBreak, lineBreak

           , text "with type:"
           , lineBreak
           , indent 1 $ ppType ty
           , lineBreak
           ]

