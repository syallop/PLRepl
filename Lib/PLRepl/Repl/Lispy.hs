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
  ( lispyExprRepl
  , plGrammarParser
  , megaparsecGrammarParser
  )
  where

import PL.Binds
import PL.Commented
import PL.Pattern
import PL.TypeCtx
import PL.Error
import PL.Expr
import PL.Hash
import PL.Kind
import PL.TyVar
import PL.Type
import PL.Var
import PL.Test.Shared
import PLGrammar
import PLLispy
import PLLispy.Level
import PLParser
import PLParser.Cursor
import PL.Store
import PLPrinter
import PL.CodeStore
import PLRepl.Repl
import qualified PLParser as PLParser

import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Control.Applicative
import Data.Void
import Reversible
import Reversible.Iso
import qualified Control.Monad.Combinators.Expr as Mega
import qualified PLGrammar as G
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega

-- | A Repl which:
-- - Parses
-- - Resolves types of referenced ContentBindings from the CodeStore
-- - Type-checks
-- - Kind-checks
-- - Reduces expressions
-- - Stores expressions, types and kinds in the provided CodeStore
-- - Evaluates the final expression
lispyExprRepl
  :: CodeStore
  -> SimpleRepl
lispyExprRepl codeStore =
  let typeCtx   = sharedTypeCtx

      read :: Text -> Repl CommentedExpr
      read txt = do
        -- TODO: Debug level
        {-
        replLog $ mconcat
          [ text "Read text:"
          , lineBreak
          , rawText txt
          , lineBreak
          ]
        -}

        readExpr <- plGrammarParser exprGrammar txt
        replLog $ mconcat
          [ text "Parsed expression (with comments hidden):"
          , lineBreak
          , indent1 . ppExpr . addComments . stripComments $ readExpr
          , lineBreak
          ]
        pure readExpr

      eval :: CommentedExpr -> Repl Expr
      eval = \commentedExpr -> do
        -- Remove things for humans
        let expr = stripComments commentedExpr

        -- Type check
        checkedType <- replResolveAndTypeCheck expr
        replLog . mconcat $
          [ lineBreak
          , text "Expression is well typed with type:"
          , lineBreak
          , indent1 . ppType . addTypeComments $ checkedType
          , lineBreak
          ]

        -- Kind check
        contentBindingKinds <- replResolveTypesTypeContentKinds checkedType
        replLog . mconcat $ if Map.null contentBindingKinds
          then mempty
          else [ lineBreak
               , text "All named types referenced at the top-level have a known kind."
               , lineBreak
               ]

        checkedKind <- replKindCheck contentBindingKinds emptyCtx checkedType
        replLog . mconcat $
          [ lineBreak
          , text "Whose type has kind:"
          , lineBreak
          , indent1 $ ppKind checkedKind
          , lineBreak
          ]

        -- Reduce
        reducedExpr <- replReduceExpr expr
        replLog $ if expr == reducedExpr
          then mconcat
            [ lineBreak
            , text "Expression does not reduce any further."
            , lineBreak
            ]

          else mconcat
            [ lineBreak
            , text "Expression reduces to:"
            , lineBreak
            , indent1 . ppExpr . addComments $ reducedExpr
            , lineBreak
            ]

        -- Store reduced expression
        storeResult <- replStore reducedExpr checkedType checkedKind
        replLog . mconcat $
          [ lineBreak
          , text "Successfully stored (against their hashes):"
          , lineBreak

          , indent1 . mconcat $
              [ text "Expression:"
              , lineBreak
              , indent1 . mconcat $
                  [ text . showBase58 . _exprHash $ storeResult
                  , case _exprResult storeResult of
                      Successfully
                        -> mempty
                      AlreadyStored
                        -> lineBreak <> text "(was already stored)"
                      -- TODO: Indicate what values were replaced
                      Overwritten _oldValues
                        -> lineBreak <> text "Replaced equivalent existing values"
                  ]
              , lineBreak
              ]

          , lineBreak
          , indent1 . mconcat $
              [ text "Type:"
              , lineBreak
              , indent1 . mconcat $
                  [ text . showBase58 . _typeHash $ storeResult
                  , case _typeResult storeResult of
                      Successfully
                        -> mempty
                      AlreadyStored
                        -> lineBreak <> text "(was already stored)"
                      -- TODO: Indicate what values were replaced
                      Overwritten _oldValues
                        -> lineBreak <> text "Replaced equivalent existing values"
                  ]
              , lineBreak
              ]

          , lineBreak
          , indent1 . mconcat $
              [ text "Kind:"
              , lineBreak
              , indent1 . mconcat $
                  [ text . showBase58 . _kindHash $ storeResult
                  , case _kindResult storeResult of
                      Successfully
                        -> mempty
                      AlreadyStored
                        -> lineBreak <> text "(was already stored)"
                      -- TODO: Indicate what values were replaced
                      Overwritten _oldValues
                        -> lineBreak <> text "Replaced equivalent existing values"
                  ]
              , lineBreak
              ]
              -- TODO: Report on type/kind associations
          ]

        -- Evaluate
        replLog . mconcat $
          [ lineBreak
          , text "Evaluating expression (non-termination is a bug!):"
          ]
        (_,evaluatedExpr) <- replEvaluateExpr reducedExpr (Just 1024)
        replLog . indent1 . mconcat $ if evaluatedExpr == reducedExpr
          then [ lineBreak
               , text "Expression does not evaluate any further."
               , lineBreak
               ]
          else [ lineBreak
               , text "Expression evaluates to:"
               , indent1 . ppExpr . addComments $ evaluatedExpr
               , lineBreak
               ]

        pure evaluatedExpr


      ctx = mkReplCtx typeCtx codeStore
   in mkSimpleRepl read eval ppResult ctx

   where
     exprGrammar :: G.Grammar CommentedExpr
     exprGrammar = top $ expr var (sub $ typ tyVar) tyVar

     ppKind    = fromMaybe mempty . pprint (toPrinter kind)
     ppType    = fromMaybe mempty . pprint (toPrinter $ top $ typ tyVar)
     ppPattern = fromMaybe mempty . pprint (toPrinter $ top $ pattern var tyVar) . addPatternComments
     ppExpr    = fromMaybe mempty . pprint (toPrinter $ top $ expr var (sub $ typ tyVar) tyVar)
     ppVar     = fromMaybe mempty . pprint (toPrinter var)
     ppTyVar   = fromMaybe mempty . pprint (toPrinter tyVar)

     ppResult :: Either (Error Expr Type Pattern TypeCtx) (CommentedExpr,Expr)
              -> Doc
     ppResult e = case e of
       Left err
         -> ppError ppPattern
                    (ppType . addTypeComments)
                    (ppExpr . addComments)
                    (ppTypeCtx document (ppTypeInfo (ppType . addTypeComments)))
                    ppVar
                    ppTyVar
                    $ err

       Right (_readExpr,_evaluatedExpr)
         -> mconcat
              [ lineBreak
              , text "Finished successfully"
              , lineBreak
              , text "You may now reference this expression by it's hash"
              , lineBreak
              ]

-- Convert a Grammar to a parser using PLParser.
plGrammarParser
  :: Grammar read
  -> Text
  -> Repl read
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

-- Pretty print a parse success or failure.
-- Optimisations specific to the lispy grammar are used.
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
              (d:_)
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
             , indent 2 $ ppCursor cur0
             , lineBreak
             , if null failures
                 then indent 2 $ mconcat
                   [ text "For no known reason. Which is likely a bug on our part"
                   , lineBreak
                   ]
                 else mconcat $
                   [ case Set.toList failureAtStop of
                       []
                         -> text "Due to backtracked error"

                       -- TODO: This occurs when backtracking from alternatives
                       -- which all fail. There may be a better way to
                       -- encapsulate this state.
                       [ExpectFail]
                         -> mempty

                       [e]
                         -> text "Where we allowed: " <> ppExpected e <> lineBreak

                       (e:es)
                         -> mconcat
                              [ text "Where we allowed:"
                              , lineBreak
                              , indent 2 $ ppExpected $ foldr PLParser.ExpectEither e es
                              ]
                   ]

              -- Describe failures we've backtracked from.
              --
              -- The deepest, and first failure seems to be the mostly likely to
              -- be the cause of an error so it is reported first.
              --
              -- TODO: Should it be highlighted more prominently?
              --
              -- It's possible shallower backtracks were the cause of an error
              -- so we include them.
              -- TODO: This can report a lot of noise so we should attempt to
              -- validate how useful these are.
              , case backtrackedFailures of
                  -- No Backtracking
                  []
                    -> mempty

                  (e:es)
                    -> mconcat
                         [ if null es
                             then text "After backtracking from:"
                             else text "After backtracking from several possibilities:"
                         , lineBreak
                         , indent 2 $ ppPossibilities (e:es)
                         , lineBreak
                         ]

              -- TODO: Is it ever helpful to show prior alternatives we moved
              -- past?
              -- If applying the suggestion makes the entire expression valid
              -- it's potentially worthwhile displaying.
              ]
  where
    ppPossibilities :: [(Cursor, Set Expected)] -> Doc
    ppPossibilities p = indent 2 $ mconcat . map (\(c,es) ->
      case Set.toList es of
        []
          -> mempty
        e:es
          -> mconcat [ ppCursor $ c
                     , lineBreak
                     , text "Expected: "
                     , ppExpected $ foldr PLParser.ExpectEither e es
                     , lineBreak
                     , lineBreak
                     ]
      ) $ p


    collectFailures :: [(Expected,Cursor)] -> Map.Map Cursor (Set Expected)
    collectFailures allFailures = foldr (\(expected,cursor) acc
                                          -> Map.insertWith (<>) cursor (Set.singleton expected) acc
                                        )
                                        mempty
                                        allFailures
    ppPos :: Pos -> Doc
    ppPos (Pos t l c) = mconcat
      [ text "Line:     ", int l, lineBreak
      , text "Character:", int c, lineBreak
      , text "Total:    ", int t, lineBreak
      ]

    ppCursor :: Cursor -> Doc
    ppCursor (Cursor prev next pos@(Pos t l c)) =
      -- TODO: This is.. suboptimal
      let (untilLineEnd,_rest) = Text.span (/= '\n') next
          beforeLines = Text.splitOn "\n" $ (Text.concat . reverse $ prev) <> untilLineEnd
          line = last beforeLines

          -- Convert from 0-index to 1-index
          lineNumber = l + 1
          charNumber = c + 1

          lineNumberSize = length $ show lineNumber
          prevGutter     = text " " <> text (Text.replicate lineNumberSize " ") <> text "│"
          gutter         = text " " <> int lineNumber <> text "│ "
          pointer        = text " " <> text (Text.replicate lineNumberSize " ") <> text "└" <> text (Text.replicate charNumber "─") <> text "┴"
       in mconcat
            [ prevGutter
            , lineBreak

            , gutter
            , rawText line
            , lineBreak

            , pointer
            ]

    ppExpected :: Expected -> Doc
    ppExpected = (\docs -> case docs of
                   []
                     -> mempty
                   ds
                     -> bulleted ds

                 )
               . flattenExpectedDoc

    -- Returns alternatives
    flattenExpectedDoc :: Expected -> [Doc]
    flattenExpectedDoc e = List.nub $ case e of
      ExpectEither es0 es1
        -> flattenExpectedDoc es0 <> flattenExpectedDoc es1

      ExpectFail
        -> []

      ExpectText txt
        -> [text txt]

      -- For a predicate with a descriptive label, the label is enough.
      ExpectPredicate (Label lTxt Descriptive) _
        -> [text $ "_PREDICATE_" <> lTxt]

      -- For an enhancing label, we still want to see the rest of the definition.
      ExpectPredicate (Label lTxt Enhancing) mE
        -> map ((text "_PREDICATE_" <> text lTxt) <>) $ maybe [] flattenExpectedDoc mE

      ExpectAnything
        -> [text "ANYTHING"]

      ExpectN i e
        -> [text $ "_EXACTLY_" <> (Text.pack . show $ i) <> "_"
           ,mconcat . flattenExpectedDoc $ e
           ]

      -- A descriptive label is sufficient.
      ExpectLabel (Label lTxt Descriptive) e
        -> [text lTxt]

      -- An enhancing label requires the rest of the definition.
      ExpectLabel (Label lTxt Enhancing) e
        -> [text $ lTxt <> " " <> (render . mconcat . flattenExpectedDoc $ e)
           ]

      ExpectThen e0 e1
        -> [ text . render . mconcat . flattenExpectedDoc $ e0
           , text "_THEN_"
           , text . render . mconcat . flattenExpectedDoc $ e1
           ]

-- Convert a Grammar to a parser using Megaparsec.
megaparsecGrammarParser
  :: Grammar read
  -> Text
  -> Repl read
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

