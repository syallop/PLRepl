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
  , TypeFamilies
  , EmptyDataDecls
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

  , exprGrammar
  , typeGrammar
  , patternGrammar

  , commentedExprGrammar
  , commentedTypeGrammar
  , commentedPatternGrammar

  , ppExpr
  , ppType
  , ppPattern

  , ppCommentedExpr
  , ppCommentedType
  , ppCommentedPattern

  , ppDefaultError

  , ppKind
  , ppVar
  , ppTyVar
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
import PL.FixPhase
import PL.Type
import PL.Var
import PL.Test.Shared
import PL.HashStore
import PL.Serialize
import PLGrammar
import PLLispy
import PLLispy.Name
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
import Data.Text.Encoding
import Data.Text (Text)
import qualified Data.Void as Void
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Control.Applicative
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

      read :: Text -> Repl (ExprFor CommentedPhase)
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

        readExpr <- plGrammarParser commentedExprGrammar txt
        replLog $ mconcat
          [ text "Parsed expression (with comments hidden):"
          , lineBreak
          , indent1 . ppCommentedExpr . stripComments $ readExpr
          , lineBreak
          ]
        pure readExpr

      eval :: ExprFor CommentedPhase -> Repl Expr
      eval = \commentedExpr -> do
        -- Remove comments
        let strippedExpr :: ExprFor Stripped
            strippedExpr = stripComments commentedExpr

        -- Resolve short hashes to unambiguous ContentName hashes
        resolvedExpr <- replResolveShortHashes strippedExpr
        replLog . mconcat $
          [ lineBreak
          , text "Resolved all short-hashes to unambiguous hashes"
          , lineBreak
          , indent1 . ppExpr $ resolvedExpr
          , lineBreak
          ]

        -- Type check
        checkedType <- replResolveAndTypeCheck resolvedExpr
        replLog . mconcat $
          [ lineBreak
          , text "Expression is well typed with type:"
          , lineBreak
          , indent1 . ppType $ checkedType
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
        checkedKind <- replKindCheck contentBindingKinds emptyCtx Nothing checkedType
        replLog . mconcat $
          [ lineBreak
          , text "Whose type has kind:"
          , lineBreak
          , indent1 $ ppKind checkedKind
          , lineBreak
          ]

        -- Reduce
        reducedExpr <- replReduceExpr resolvedExpr
        replLog $ if resolvedExpr == reducedExpr
          then mconcat
            [ lineBreak
            , text "Expression does not reduce any further."
            , lineBreak
            ]

          else mconcat
            [ lineBreak
            , text "Expression reduces to:"
            , lineBreak
            , indent1 . ppExpr $ reducedExpr
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
                  [ text . showBase58 . _storedAgainstHash . _exprResult $ storeResult
                  , case _storeResult . _exprResult $ storeResult of
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
                  [ text . showBase58 . _storedAgainstHash . _typeResult $ storeResult
                  , case _storeResult . _typeResult $ storeResult of
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
                  [ text . showBase58 . _storedAgainstHash . _kindResult $ storeResult
                  , case _storeResult . _kindResult $ storeResult of
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
               , indent1 . ppExpr $ evaluatedExpr
               , lineBreak
               ]

        pure evaluatedExpr

      ctx = mkReplCtx typeCtx codeStore
   in mkSimpleRepl read eval ppResult ctx

   where
     ppResult :: Either Error (ExprFor CommentedPhase,Expr)
              -> Doc
     ppResult e = case e of
       Left err
         -> ppError ppDefaultError $ err

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
    toParser :: G.Grammar a -> Mega.Parsec Void.Void Text a
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
      -> Mega.Parsec Void.Void Text b
    rmapParser iso g = do
      a <- toParser g
      case forwards iso a of
        Nothing
          -> fail "iso"
        Just b
          -> pure b

    rapParser
      :: Mega.Parsec Void.Void Text a
      -> Mega.Parsec Void.Void Text b
      -> Mega.Parsec Void.Void Text (a, b)
    rapParser pl pr = (,) <$> pl <*> pr

noExtG :: Grammar NoExt
noExtG = rpure noExt

exprGrammar :: Grammar Expr
exprGrammar = top $ expr exprDeps typeDeps patternDeps

typeGrammar :: Grammar Type
typeGrammar = top $ typ typeDeps

patternGrammar :: Grammar Pattern
patternGrammar = top $ pattern patternDeps typeDeps

commentedExprGrammar :: Grammar (ExprFor CommentedPhase)
commentedExprGrammar = top $ expr commentedExprDeps commentedTypeDeps commentedPatternDeps

commentedTypeGrammar :: Grammar (TypeFor CommentedPhase)
commentedTypeGrammar = top $ typ commentedTypeDeps

commentedPatternGrammar :: Grammar (PatternFor CommentedPhase)
commentedPatternGrammar = top $ pattern commentedPatternDeps commentedTypeDeps

exprDeps :: GrammarDependencies DefaultPhase
exprDeps = GrammarDependencies
  { _bindingFor                = var
  , _contentBindingFor         = contentNameGrammar
  , _abstractionFor            = sub $ typ typeDeps
  , _exprTypeBindingFor        = tyVar
  , _exprTypeContentBindingFor = contentNameGrammar

  , _lamGrammarExtension            = noExtG
  , _appGrammarExtension            = noExtG
  , _bindingGrammarExtension        = noExtG
  , _contentBindingGrammarExtension = noExtG
  , _caseAnalysisGrammarExtension   = noExtG
  , _sumGrammarExtension            = noExtG
  , _productGrammarExtension        = noExtG
  , _unionGrammarExtension          = noExtG
  , _bigLamGrammarExtension         = noExtG
  , _bigAppGrammarExtension         = noExtG

  , _exprGrammarExtension = rempty
  }

typeDeps :: TypeGrammarDependencies DefaultPhase
typeDeps = TypeGrammarDependencies
  { _typeBindingFor        = tyVar
  , _typeContentBindingFor = contentNameGrammar

  , _namedGrammarExtension              = noExtG
  , _arrowGrammarExtension              = noExtG
  , _sumTGrammarExtension               = noExtG
  , _productTGrammarExtension           = noExtG
  , _unionTGrammarExtension             = noExtG
  , _bigArrowGrammarExtension           = noExtG
  , _typeLamGrammarExtension            = noExtG
  , _typeAppGrammarExtension            = noExtG
  , _typeBindingGrammarExtension        = noExtG
  , _typeContentBindingGrammarExtension = noExtG

  , _typeGrammarExtension = rempty
  }

patternDeps :: PatternGrammarDependencies DefaultPhase
patternDeps = PatternGrammarDependencies
  { _patternBindingFor = var

  , _sumPatternGrammarExtension     = noExtG
  , _productPatternGrammarExtension = noExtG
  , _unionPatternGrammarExtension   = noExtG
  , _bindingPatternGrammarExtension = noExtG
  , _bindGrammarExtension           = noExtG

  , _patternGrammarExtension = rempty
  }

commentedExprDeps :: GrammarDependencies CommentedPhase
commentedExprDeps = GrammarDependencies
  { _bindingFor                = var
  , _contentBindingFor         = shortHash
  , _abstractionFor            = sub $ typ commentedTypeDeps
  , _exprTypeBindingFor        = tyVar
  , _exprTypeContentBindingFor = shortHash

  , _lamGrammarExtension            = noExtG
  , _appGrammarExtension            = noExtG
  , _bindingGrammarExtension        = noExtG
  , _contentBindingGrammarExtension = noExtG
  , _caseAnalysisGrammarExtension   = noExtG
  , _sumGrammarExtension            = noExtG
  , _productGrammarExtension        = noExtG
  , _unionGrammarExtension          = noExtG
  , _bigLamGrammarExtension         = noExtG
  , _bigAppGrammarExtension         = noExtG

  , _exprGrammarExtension = commentedExpr commentedExprDeps commentedTypeDeps commentedPatternDeps
  }

commentedTypeDeps :: TypeGrammarDependencies CommentedPhase
commentedTypeDeps = TypeGrammarDependencies
  { _typeBindingFor        = tyVar
  , _typeContentBindingFor = shortHash

  , _namedGrammarExtension              = noExtG
  , _arrowGrammarExtension              = noExtG
  , _sumTGrammarExtension               = noExtG
  , _productTGrammarExtension           = noExtG
  , _unionTGrammarExtension             = noExtG
  , _bigArrowGrammarExtension           = noExtG
  , _typeLamGrammarExtension            = noExtG
  , _typeAppGrammarExtension            = noExtG
  , _typeBindingGrammarExtension        = noExtG
  , _typeContentBindingGrammarExtension = noExtG

  , _typeGrammarExtension = commentedTyp commentedTypeDeps
  }

commentedPatternDeps :: PatternGrammarDependencies CommentedPhase
commentedPatternDeps = PatternGrammarDependencies
  { _patternBindingFor = var

  , _sumPatternGrammarExtension     = noExtG
  , _productPatternGrammarExtension = noExtG
  , _unionPatternGrammarExtension   = noExtG
  , _bindingPatternGrammarExtension = noExtG
  , _bindGrammarExtension           = noExtG

  , _patternGrammarExtension = commentedPattern commentedPatternDeps commentedTypeDeps
  }

ppCommentedExpr :: ExprFor CommentedPhase -> Doc
ppCommentedExpr = fromMaybe mempty . pprint (toPrinter commentedExprGrammar)

ppCommentedType :: TypeFor CommentedPhase -> Doc
ppCommentedType = fromMaybe mempty . pprint (toPrinter commentedTypeGrammar)

ppCommentedPattern :: PatternFor CommentedPhase -> Doc
ppCommentedPattern = fromMaybe mempty . pprint (toPrinter commentedPatternGrammar)

ppExpr :: Expr -> Doc
ppExpr = fromMaybe mempty . pprint (toPrinter exprGrammar)

ppType :: Type -> Doc
ppType = fromMaybe mempty . pprint (toPrinter typeGrammar)

ppPattern :: Pattern -> Doc
ppPattern = fromMaybe mempty . pprint (toPrinter patternGrammar)

ppVar :: Var -> Doc
ppVar = fromMaybe mempty . pprint (toPrinter var)

ppTyVar :: TyVar -> Doc
ppTyVar = fromMaybe mempty . pprint (toPrinter tyVar)

ppDefaultError :: PPError DefaultPhase
ppDefaultError = PPError
    { _ppExpr        = ppExpr
    , _ppType        = ppType
    , _ppPattern     = ppPattern
    , _ppKind        = ppKind
    , _ppTypeCtx     = ppTypeCtx document ppType
    , _ppTypeName    = document
    , _ppBinding     = ppVar
    , _ppTypeBinding = ppTyVar
    }

ppKind = fromMaybe mempty . pprint (toPrinter kind)

data Stripped
type instance ContentBindingFor Stripped = ShortHash
type instance TypeContentBindingFor Stripped = ShortHash
type instance TypeExtension Stripped = NoExt
type instance ExprExtension Stripped = NoExt
type instance PatternExtension Stripped = NoExt
type instance BindingFor Stripped = Var
type instance TypeBindingFor Stripped = TyVar
type instance AbstractionFor Stripped = TypeFor Stripped
type instance NamedExtension Stripped = NoExt
type instance ArrowExtension Stripped = NoExt
type instance SumTExtension Stripped = NoExt
type instance ProductTExtension Stripped = NoExt
type instance UnionTExtension Stripped = NoExt
type instance BigArrowExtension Stripped = NoExt
type instance TypeLamExtension Stripped = NoExt
type instance TypeAppExtension Stripped = NoExt
type instance TypeBindingExtension Stripped = NoExt
type instance TypeContentBindingExtension Stripped = NoExt
type instance LamExtension Stripped = NoExt
type instance AppExtension Stripped = NoExt
type instance BindingExtension Stripped = NoExt
type instance ContentBindingExtension Stripped = NoExt
type instance CaseAnalysisExtension Stripped = NoExt
type instance SumExtension Stripped = NoExt
type instance ProductExtension Stripped = NoExt
type instance UnionExtension Stripped = NoExt
type instance BigLamExtension Stripped = NoExt
type instance BigAppExtension Stripped = NoExt
type instance SumPatternExtension Stripped = NoExt
type instance ProductPatternExtension Stripped = NoExt
type instance UnionPatternExtension Stripped = NoExt
type instance BindingPatternExtension Stripped = NoExt
type instance BindExtension Stripped = NoExt

-- Hijack the Lispy Parser/ Printer to define a missing serialize instance for
-- expressions to allow us to store and retrieve them from the filesystem.
--
-- Note that this means we cannot share filestores generated with this instance
-- with anything using a different orphan instance.
--
-- TODO: Remove when Exprs gain a canonical storage format.
instance Serialize Expr where
  serialize expr = case pprint (toPrinter exprGrammar) expr of
    Nothing
      -> error "Failed to Serialize an expression via a pretty-printer"

    Just doc
      -> encodeUtf8 . PLPrinter.render $ doc

  -- TODO: It might be nice to be able to fail with a reason when serialization
  -- is unsuccessful.
  deserialize bs = case PLParser.runParser (toParser exprGrammar) $ decodeUtf8 bs of
    PLParser.ParseFailure _expected _cursor
      -- TODO: Propagate expected and cursor into error message
      -> Left . EMsg . text $ "Failed to deserialize expression"

    PLParser.ParseSuccess expr cursor
      | noTrailingCharacters $ PLParser.remainder cursor
      -> Right expr

      | otherwise
      -> Left . EMsg . mconcat $
           [ text "Parsed expression:"
           , lineBreak
           , fromMaybe mempty . pprint (toPrinter exprGrammar) $ expr
           , lineBreak
           , text "But there were unexpected trailing characters:"
           , lineBreak
           , string . show . PLParser.remainder $ cursor
           ]
    where
      noTrailingCharacters :: Text -> Bool
      noTrailingCharacters txt = Text.null txt || Text.all (`elem` [' ','\t','\n','\r']) txt


instance Serialize (ExprFor CommentedPhase) where
  serialize expr = case pprint (toPrinter commentedExprGrammar) expr of
    Nothing
      -> error "Failed to Serialize an expression via a pretty-printer"

    Just doc
      -> encodeUtf8 . PLPrinter.render $ doc

  -- TODO: It might be nice to be able to fail with a reason when serialization
  -- is unsuccessful.
  deserialize bs = case PLParser.runParser (toParser commentedExprGrammar) $ decodeUtf8 bs of
    PLParser.ParseFailure _expected _cursor
      -> Left . EMsg . text $ "Failed to deserialize expression"
    PLParser.ParseSuccess expr cursor
      | noTrailingCharacters $ PLParser.remainder cursor
      -> Right expr

      | otherwise
      -> Left . EMsg . text $ "Failed to deserialize expression as there were unexpected trailing characters"
    where
      noTrailingCharacters :: Text -> Bool
      noTrailingCharacters txt = Text.null txt || Text.all (`elem` [' ','\t','\n','\r']) txt

instance Serialize Type where
  serialize typ = case pprint (toPrinter typeGrammar) typ of
    Nothing
      -> error "Failed to Serialize a type via a pretty-printer"

    Just doc
      -> encodeUtf8 . PLPrinter.render $ doc

  -- TODO: It might be nice to be able to fail with a reason when serialization
  -- is unsuccessful.
  deserialize bs = case PLParser.runParser (toParser typeGrammar) $ decodeUtf8 bs of
    PLParser.ParseFailure _expected _cursor
      -- TODO: Propagate expected and cursor into error message
      -> Left . EMsg . text $ "Failed to deserialize type"

    PLParser.ParseSuccess typ cursor
      | noTrailingCharacters $ PLParser.remainder cursor
      -> Right typ

      | otherwise
      -> Left . EMsg . text $ "Failed to deserialize type as there were unexpected trailing characters"
    where
      noTrailingCharacters :: Text -> Bool
      noTrailingCharacters txt = Text.null txt || Text.all (`elem` [' ','\t','\n','\r']) txt

instance Serialize (TypeFor CommentedPhase) where
  serialize typ = case pprint (toPrinter commentedTypeGrammar) typ of
    Nothing
      -> error "Failed to Serialize a type via a pretty-printer"

    Just doc
      -> encodeUtf8 . PLPrinter.render $ doc

  -- TODO: It might be nice to be able to fail with a reason when serialization
  -- is unsuccessful.
  deserialize bs = case PLParser.runParser (toParser commentedTypeGrammar) $ decodeUtf8 bs of
    PLParser.ParseFailure _expected _cursor
      -- TODO: Propagate expected and cursor into error message
      -> Left . EMsg . text $ "Failed to deserialize type"

    PLParser.ParseSuccess typ cursor
      | noTrailingCharacters $ PLParser.remainder cursor
      -> Right typ

      | otherwise
      -> Left . EMsg . text $ "Failed to deserialize type as there were unexpected trailing characters"
    where
      noTrailingCharacters :: Text -> Bool
      noTrailingCharacters txt = Text.null txt || Text.all (`elem` [' ','\t','\n','\r']) txt

instance Serialize Kind where
  serialize k = case pprint (toPrinter kind) k of
    Nothing
      -> error "Failed to Serialize a kind via a pretty-printer"

    Just doc
      -> encodeUtf8 . PLPrinter.render $ doc

  -- TODO: It might be nice to be able to fail with a reason when serialization
  -- is unsuccessful.
  deserialize bs = case PLParser.runParser (toParser kind) $ decodeUtf8 bs of
    PLParser.ParseFailure _expected _cursor
      -- TODO: Propagate expected and cursor into error message
      -> Left . EMsg . text $ "Failed to deserialize kind"

    PLParser.ParseSuccess kind cursor
      | noTrailingCharacters $ PLParser.remainder cursor
      -> Right kind

      | otherwise
      -> Left . EMsg . text $ "Failed to deserialize kind as there were unexpected trailing characters"
    where
      noTrailingCharacters :: Text -> Bool
      noTrailingCharacters txt = Text.null txt || Text.all (`elem` [' ','\t','\n','\r']) txt


