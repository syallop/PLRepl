{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : PLRepl.Repl
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

Thid module can be used to implement a Read Eval Print Loop for some PL repl

SimpleRepl accepts configuration in the form of ReadEval and Print functions and can
be driven with Step. This structure is implemented in terms of Repl which can be
used to build more complex repls.

Repls can be defined using a number of functions which wrap core PL operations such as:
- Resolving
- TypeChecking
- Reducing
- Storage/ Lookup
- Evaluation

Configuration(s) are found under PLRepl.Repl.*

This abstraction exists to make it easier to define multiple repls which can be
switched between. For example we may want repls for:
- Different expression grammars (E.G. Lispy vs Hasky)
- Exclusively handling type signatures
- Accepting new Type definitions
- Experimenting with pattern matches
- Different underlying implementations of E.G. the parser combinator library
|-}
module PLRepl.Repl
  (
    -- * Consumption API
    --
    -- | Run Repls with runRepl, execute the contained read,eval,print
    -- steps with replStep or individually with repl{Read,Eval,Print}.
    Repl ()
  , runRepl

  , ReplCtx (..)
  , mkReplCtx

  , SimpleRepl ()
  , mkSimpleRepl
  , step
  , simpleReplCtx

  -- * Definition API
  --
  -- | Can be used to build read,eval,print pipeline or just called manually.

  -- * Core
  --
  -- | Functions on the repl itself or its context.
  , replError
  , replIO
  , replLog
  , replModifyCtx

  -- * Resolving
  --
  -- | Resolve names external to an AST that may be used during typechecking
  --
  -- Ironically these function names themselves are bad.
  -- This is because the types arent specific enough to rule out misuse so care
  -- needs to be taken choosing the right function for the expected sort of
  -- contentname.

  -- ** Resolve Hashes to their content/ things they relate to
  , replGatherExprsExprContentNames
  , replGatherExprsTypeContentNames
  , replGatherTypesTypeContentNames

  , replResolveExprContentTypeHashes
  , replResolveTypeContentKindHashes

  , replResolveExprsExprContentTypes
  , replResolveTypesTypeContentKinds
  , replResolveExprsTypeContentTypes

  -- ** Map between short and long hashes
  , replResolveShortHashes
  , replShortenHashes

  , replResolveExprHash
  , replResolveTypeHash
  , replResolveKindHash

  , replShortenExprHash
  , replShortenTypeHash
  , replShortenKindHash

  -- * Type checking
  --
  -- | Type/ kind check expressions/ types to prove they're well formed to be
  -- reduced/ evaluated
  , replResolveAndTypeCheck
  , replTypeCheck
  , replKindCheck

  -- * Reduction
  --
  -- | Reduce valid expressions and types to their normal form, suitable to be
  -- stored.
  , replReduceExpr
  , replReduceType

  -- * Storage
  --
  -- | Store typechecked, reduced expressions, in the CodeStore as well as
  -- types, kinds and relations such as expr-has-type and type-has-kind.
  , ReplStoreResult (..)
  , replStore

  , replStoreExpr
  , replStoreType
  , replStoreKind
  , replStoreExprHasType
  , replStoreTypeHasKind

  -- * Lookup
  --
  -- | Retrieve stored expressions from the CodeStore as well as types, kinds
  -- and relations such as expr-has-type and type-has-kind.
  , replLookup
  , replLookupExpr
  , replLookupType
  , replLookupKind
  , replLookupExprsType
  , replLookupTypesKind

  -- * Evaluation
  --
  -- | Evaluate type-checked, reduced expressions to produce their final result.
  , replEvaluateExpr
  , replEvaluateType
  )
  where

import PL.Binds
import PL.Error
import PL.Evaluate
import PL.Expr
import PL.Hash
import PL.Kind
import PL.Name
import PL.Pattern
import PL.Reduce
import PL.ReduceType
import PL.Store
import PL.HashStore
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCheck
import PL.TypeCtx
import PL.Resolve
import qualified PL.CodeStore as CodeStore

import qualified PLParser as PLParser
import PLPrinter
import PLGrammar

import Control.Monad
import Data.Foldable
import Data.List (intercalate,intersperse)
import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Encapsulates a read, eval and print function alongside the current ReplCtx.
-- A SimpleRepl can then be 'step'ed to run one iteration of the loop.
--
-- More complex interaction could be built directly with Repl.
data SimpleRepl = forall read eval. SimpleRepl
  { _readEval    :: Text -> Repl (read,eval)
  , _prettyPrint :: Either (Error Expr Type Pattern TypeCtx) (read,eval) -> Doc
  , _ctx         :: ReplCtx
  }

-- | Create a simple repl from a read, eval and print function as well as an
-- initial context.
mkSimpleRepl
  :: (Text -> Repl read)
  -> (read -> Repl eval)
  -> (Either (Error Expr Type Pattern TypeCtx) (read,eval) -> Doc)
  -> ReplCtx
  -> SimpleRepl
mkSimpleRepl read eval pprint ctx = SimpleRepl
  { _readEval = \input -> do
      r <- read input
      e <- eval r
      pure (r,e)
  , _prettyPrint = pprint
  , _ctx = ctx
  }

-- | Extract the current context of a SimpleRepl.
simpleReplCtx
  :: SimpleRepl
  -> ReplCtx
simpleReplCtx (SimpleRepl _ _ ctx) = ctx

-- | Feed input into a SimpleRepl in order to evaluate a single step.
--
-- The result contains:
-- - A log of operations formatted as a Doc
-- - Either:
--   - An Error that occured somewhere in reading/ evaluating
--   - The next SimpleRepl - indicating success -
step
  :: SimpleRepl
  -> Text
  -> IO (Doc, Either (Error Expr Type Pattern TypeCtx) SimpleRepl)
step (SimpleRepl readEval print ctx) txt = do
  (ctx',log,res) <- runRepl ctx $ readEval txt
  case res of
    Left err
      -> pure . (log,) . Left $ err

    Right _
      -> pure . (log <> lineBreak <> print res,) . Right $ SimpleRepl readEval print ctx'

-- | Context Repl's execute under.
--
-- Contains:
-- - State that computations may access, such as type contexts' and
--   codestores.
-- - Configured Read, Eval, Print functions that Repl functions may use.
data ReplCtx = ReplCtx
  { _replTypeCtx   :: TypeCtx             -- ^ The type definitions in scope
  , _replCodeStore :: CodeStore.CodeStore -- ^ Access to a CodeStore for lookup and storage
  }

-- | Create a repl context with some initial state.
mkReplCtx
  :: TypeCtx
  -> CodeStore.CodeStore
  -> ReplCtx
mkReplCtx typeCtx codeStore = ReplCtx
  { _replTypeCtx   = typeCtx
  , _replCodeStore = codeStore
  }

-- | Repls are functions which:
-- - Have access to some ReplState which can change under success or failure
-- - May fail with an Error
-- - May succeed with a Value
-- - Accumulates a log of what it did as a Doc
newtype Repl a = Repl
  {_unRepl :: ReplCtx
           -> IO ( ReplCtx
                 , Doc
                 , Either (Error Expr Type Pattern TypeCtx)
                           a
                 )
  }

instance Functor Repl where
  fmap f (Repl r) = Repl $ \st -> do
    (st',log,res) <- r st
    pure (st', log, case res of
      Left err -> Left err
      Right a  -> Right $ f a)

instance Applicative Repl where
  pure = return
  (<*>) = ap

instance Monad Repl where
  return a = Repl $ \st -> pure (st,mempty,Right a)

  (Repl f) >>= fab = Repl $ \st -> do
    (st',log,res) <- f st
    case res of
      Left err -> pure (st',log,Left err)
      Right a  -> do let Repl g = fab a
                     (st'',log',res') <- g st'
                     pure (st'',log<>log',res')


-- | Inject an error into the repl
replError
  :: Error Expr Type Pattern TypeCtx
  -> Repl x
replError err = Repl $ \st -> pure (st, mempty, Left err)

-- | Lift an IO action into the Repl
replIO
  :: IO a
  -> Repl a
replIO f = Repl $ \st -> f >>= \a -> pure (st, mempty, Right a)

-- | Append a log line to the repl to be accessible at the end of execution.
replLog
  :: Doc
  -> Repl ()
replLog d = Repl $ \st -> pure (st, d, Right ())

-- | Retrieve the codestore
replCodeStore
  :: Repl CodeStore.CodeStore
replCodeStore = Repl $ \st -> pure (st, mempty, Right $ _replCodeStore st)

-- | Retrieve the type context
replTypeCtx
  :: Repl TypeCtx
replTypeCtx = Repl $ \st -> pure (st, mempty, Right $ _replTypeCtx st)

-- | Mutate the underlying ReplCtx
replModifyCtx
  :: (ReplCtx -> ReplCtx)
  -> Repl ()
replModifyCtx f = Repl $ \ctx -> pure (f ctx, mempty, Right ())


{- Consumption API -}

-- | Execute a Repl function to produce its final state and either an error or a
-- successful result.
runRepl
  :: ReplCtx
  -> Repl a
  -> IO ( ReplCtx
        , Doc
        , Either (Error Expr Type Pattern TypeCtx)
                 a
        )
runRepl ctx r = _unRepl r ctx

{- Definition API -}

{-  Resolving -}

-- TODO: A lot of logic here might belong in the Resolve phase.

-- | Gather all top-level names that refer to external expressions.
replGatherExprsExprContentNames
  :: Expr
  -> Repl (Set ContentName)
replGatherExprsExprContentNames = pure . gatherContentNames

-- | Gather all top-level names that refer to external types.
replGatherExprsTypeContentNames
  :: Expr
  -> Repl (Set ContentName)
replGatherExprsTypeContentNames = pure . gatherExprsTypeContentNames

-- | Gather all top-level names that refer to external types.
replGatherTypesTypeContentNames
  :: Type
  -> Repl (Set ContentName)
replGatherTypesTypeContentNames = pure . gatherTypeContentNames


-- | Resolve all expression ContentNames to their type hash.
--
-- Fails if any name does not resolve.
replResolveExprContentTypeHashes
  :: Set ContentName
  -> Repl (Map ContentName Hash)
replResolveExprContentTypeHashes =
  fmap Map.fromList . mapM (\h -> do mTy <- replLookupExprsType . contentName $ h
                                     case mTy of
                                       Nothing
                                         -> replError . EMsg . mconcat $
                                              [ text "When resolving a set of expression ContentNames we failed to lookup a type for:"
                                              , lineBreak
                                              , string . show $ h
                                              ]
                                       Just ty
                                         -> pure (h,ty)
                           )
                    . Set.toList

-- Fails if any name does not resolve.
replResolveTypeContentTypes
  :: Set ContentName
  -> Repl (Map ContentName Type)
replResolveTypeContentTypes =
  fmap Map.fromList . mapM (\h -> do mTy <- replLookupType . contentName $ h
                                     case mTy of
                                       Nothing
                                         -> replError . EMsg . mconcat $
                                              [ text "When resolving a set of type ContentNames we failed to lookup a type for:"
                                              , lineBreak
                                              , string . show $ h
                                              ]
                                       Just ty
                                         -> pure (h,ty)
                           )
                    . Set.toList
  --fmap Map.fromList . mapM (undefined :: ContentName -> Repl (ContentName, Type))

-- | Resolve all type ContentNames to their kind hash.
--
-- Fails if any name does not resolve.
replResolveTypeContentKindHashes
  :: Set ContentName
  -> Repl (Map ContentName Hash)
replResolveTypeContentKindHashes =
  fmap Map.fromList . mapM (\h -> do mKind <- replLookupTypesKind . contentName $ h
                                     case mKind of
                                       Nothing
                                         -> replError . EMsg . mconcat $
                                              [ text "When resolving a set of type ContentNames we failed to lookup a kind for:"
                                              , lineBreak
                                              , string . show $ h
                                              ]
                                       Just kind
                                         -> pure (h,kind)
                           )
                    . Set.toList

-- | Resolving checks every top-level referenced expression:
-- - Has an associated type hash
-- - Whose type hash has an associated type
--
-- And if so returns those associations.
replResolveExprsExprContentTypes
  :: Expr
  -> Repl (Map ContentName Type)
replResolveExprsExprContentTypes expr = do
  replGatherExprsExprContentNames expr
    >>= replResolveExprContentTypeHashes
    >>= mapM (\typeHash -> do mTy <- replLookupType typeHash
                              case mTy of
                                -- TODO: More context on the outer expression
                                -- and the expression hash we're looking at
                                -- would be helpful.
                                Nothing
                                  -> replError . EMsg . mconcat $
                                       [ text "Inside an expression we found an expression hash with a type hash, however we failed to resolve that type hash to a type:"
                                       , lineBreak
                                       , string . show $ typeHash
                                       ]
                                Just ty
                                  -> pure ty
              )

-- | Resolving checks every top-level referenced type:
-- - Has an associated kind hash
-- - Whose kind hash has an associated kind
--
-- And if so returns those associations.
replResolveTypesTypeContentKinds
  :: Type
  -> Repl (Map ContentName Kind)
replResolveTypesTypeContentKinds typ =
  replGatherTypesTypeContentNames typ
    >>= replResolveTypeContentKindHashes
    >>= mapM (\kindHash -> do mKind <- replLookupKind kindHash
                              case mKind of
                                -- TODO: More context on the outer type
                                -- and the type hash we're looking at
                                -- would be helpful.
                                Nothing
                                  -> replError . EMsg . mconcat $
                                       [ text "Inside a type we found an type hash with a kind hash, however we failed to resolve that kind hash to a kind:"
                                       , lineBreak
                                       , string . show $ kindHash
                                       ]
                                Just kind
                                  -> pure kind
             )

-- | Resolving checks every top-level referenced type has an associated type and
-- returns the associations.
replResolveExprsTypeContentTypes
  :: Expr
  -> Repl (Map ContentName Type)
replResolveExprsTypeContentTypes expr =
  replGatherExprsTypeContentNames expr
    >>= replResolveTypeContentTypes

{-  Type checking -}

-- | Check that a top-level expression has a valid type
-- , after resolving content-bindings types from the codestore.
replResolveAndTypeCheck
  :: Expr
  -> Repl Type
replResolveAndTypeCheck expr = do
  exprContentHasType <- replResolveExprsExprContentTypes expr
  typeContentIsType  <- replResolveExprsTypeContentTypes expr
  replTypeCheck exprContentHasType typeContentIsType expr

-- | Check that a top-level expression has a valid type.
replTypeCheck
  :: Map ContentName Type -- ^ Map expression content-bindings to the type they _have_
  -> Map ContentName Type -- ^ Map type content-bindings to the type they _are_
  -> Expr
  -> Repl Type
replTypeCheck exprContentHasType typeContentIsType expr = do
  typeCtx <- replTypeCtx
  let typeCheckCtx = (topTypeCheckCtx typeCtx)
        { _contentHasType = exprContentHasType
        , _contentIsType  = typeContentIsType
        }
  case exprType typeCheckCtx expr of
    Left err
      -> replError $ EContext (EMsg $ text "Type-checking top-level expression") $ err

    Right ty
      -> pure ty

-- | Check that a type has a valid kind under some bindings.
replKindCheck
  :: Map ContentName Kind
  -> BindCtx TyVar Kind
  -> Type
  -> Repl Kind
replKindCheck contentBindingKinds typeKinds typ = do
  typeCtx <- replTypeCtx
  case typeKind typeKinds contentBindingKinds typeCtx typ of
    Left err
      -> replError $ EContext (EMsg $ text "Kind-checking top-level type") $ err

    Right kind
      -> pure kind

{-  Reducing -}

-- | Reduce a top-level expression to its normal form means:
--
-- - Variable bindings are substituted under known function applications
-- - Known case statements reduce to their matched branches
-- - Named expressions are _not_ substituted
replReduceExpr
  :: Expr
  -> Repl Expr
replReduceExpr expr = do
  typeCtx <- replTypeCtx
  case reduce (topReductionCtx typeCtx) expr of
    Left err
      -> replError $ EContext (EMsg $ text "Reducing top-level expression") $ err

    Right reducedExpr
      -> pure reducedExpr

-- | Reduce a top-level type to its normal form means:
--
-- - Type bindings are substituted under known type function applications
replReduceType
  :: Type
  -> Repl Type
replReduceType typ = do
  typeCtx <- replTypeCtx
  case reduceType (topTypeReductionCtx typeCtx) typ of
    Left err
      -> replError $ EContext (EMsg $ text "Reducing top-level type") $ err

    Right reducedType
      -> pure reducedType

{-  Storing -}

-- | Store an expression by it's hash.
--
-- The returned hash can be used to retrieve the Expr.
--
-- The StoreResult indicates different forms of success, E.G. Whether:
-- - An old Expr was replaced.
-- - An identical expression was already stored.
--
-- Failures are indicated in the underlying Error type.
replStoreExpr
  :: Expr
  -> Repl (StoreResult Expr, Hash)
replStoreExpr expr = do
  codeStore <- replCodeStore
  eRes <- replIO $ CodeStore.storeExpr codeStore expr
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Failed to store expression") $ err

    Right (codeStore', storeResult, hash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure (storeResult, hash)

-- | Store a type by it's hash.
--
-- The returned hash can be used to retrieve the Type.
--
-- The StoreResult indicates different forms of success, E.G. Whether:
-- - An old type was replaced.
-- - An identical type was already stored.
--
-- Failures are indicated in the underlying Error type.
replStoreType
  :: Type
  -> Repl (StoreResult Type, Hash)
replStoreType typ = do
  codeStore <- replCodeStore
  eRes <- replIO $ CodeStore.storeType codeStore typ
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Failed to store type") $ err

    Right (codeStore', storeResult, hash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure (storeResult, hash)

-- | Store a kind by it's hash.
--
-- The returned hash can be used to retrieve the Kind.
--
-- The StoreResult indicates different forms of success, E.G. Whether:
-- - An old kind was replaced.
-- - An identical kind was already stored.
--
-- Failures are indicated in the underlying Error type.
replStoreKind
  :: Kind
  -> Repl (StoreResult Kind, Hash)
replStoreKind kind = do
  codeStore <- replCodeStore
  eRes <- replIO $ CodeStore.storeKind codeStore kind
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Failed to store kind") $ err

    Right (codeStore', storeResult, hash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure (storeResult, hash)

-- | Store a promise that an expression referred to by a hash has a type given
-- by a hash.
--
-- The StoreResult indicates different forms of success, E.G. Whether:
-- - An old Type was replaced.
-- - An identical relation was already stored.
--
-- Failures are indicated in the underlying Error type.
replStoreExprHasType
  :: (Hash, Hash)
  -> Repl (StoreResult Hash)
replStoreExprHasType (exprHash,typeHash) = do
  codeStore <- replCodeStore
  eRes <- replIO $ CodeStore.storeExprHasType codeStore (exprHash, typeHash)
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Failed to store expression-has-type relation") $ err

    Right (codeStore', storeResult)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure storeResult

-- | Store a promise that a type referred to by a hash has a kind given
-- by a hash.
--
-- The StoreResult indicates different forms of success, E.G. Whether:
-- - An old Kind was replaced.
-- - An identical relation was already stored.
--
-- Failures are indicated in the underlying Error type.
replStoreTypeHasKind
  :: (Hash, Hash)
  -> Repl (StoreResult Hash)
replStoreTypeHasKind (typeHash,kindHash) = do
  codeStore <- replCodeStore
  eRes <- replIO $ CodeStore.storeTypeHasKind codeStore (typeHash, kindHash)
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Failed to store type-has-kind relation") $ err

    Right (codeStore', storeResult)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure storeResult

-- | Store a typed expression means to:
-- - Store the expression
-- - Store the type
-- - Store the expression-has-type relation
--
-- The returned hashes can be used to lookup the expression and type
-- respectively.
--
-- It is the callers responsibility to ensure the expression has been type
-- checked to have the provided type.
--
-- The StoreResult indicates different forms of success, E.G. Whether:
-- - An old entity was replaced.
-- - An identical entity was already stored.
--
-- Failures are indicated in the underlying Error type.
replStore
  :: Expr
  -> Type
  -> Kind
  -> Repl ReplStoreResult
replStore expr typ kind = do
  (exprResult,exprHash) <- replStoreExpr expr
  (typeResult,typeHash) <- replStoreType typ
  (kindResult,kindHash) <- replStoreKind kind
  hasTypeRes <- replStoreExprHasType (exprHash,typeHash)
  hasKindRes <- replStoreTypeHasKind (typeHash,kindHash)
  pure $ ReplStoreResult
    { _exprHash = exprHash
    , _typeHash = typeHash
    , _kindHash = kindHash

    , _exprResult = exprResult
    , _typeResult = typeResult
    , _kindResult = kindResult
    , _exprHasTypeResult = hasTypeRes
    , _typeHasKindResult = hasKindRes
    }

-- | The result of storing an expr : type : kind
-- Contains:
-- - The hashes of the stored {expr,type,kind}s
-- - The success result of storing each {expr,type,Kind} (I.E. whether an old value was replaced).
-- - The success result of storing the expr-has-type and type-has-kind relation (I.E. whether an old relation was replaced).
data ReplStoreResult = ReplStoreResult
  { _exprHash :: Hash
  , _typeHash :: Hash
  , _kindHash :: Hash

  , _exprResult :: StoreResult Expr
  , _typeResult :: StoreResult Type
  , _kindResult :: StoreResult Kind
  , _exprHasTypeResult :: StoreResult Hash
  , _typeHasKindResult :: StoreResult Hash
  }

{-  Lookup -}

-- | Lookup an Expression, it's Type and it's Types Kind by the expressions
-- Hash.
--
-- Hashes may be acquired from a prior replStore or found as a ContentBinding
-- inside an expression.
replLookup
  :: Hash
  -> Repl (Expr,Type,Kind)
replLookup exprHash = do
  let errCtx = EContext (EMsg . mconcat $ [ text "Looking up expression, type and kind associated with expression hash:"
                                          , lineBreak
                                          , string . show $ exprHash
                                          ])

  mExpr <- replLookupExpr exprHash
  expr  <- maybe (replError . errCtx . EMsg . text $ "Failed to resolve expression hash to expression") pure mExpr

  mTypeHash <- replLookupExprsType exprHash
  typeHash  <- maybe (replError . errCtx . EMsg . text $ "Failed to resolve expression hash to it's type hash") pure mTypeHash

  mTyp <- replLookupType typeHash
  typ  <- maybe (replError . errCtx . EMsg . text $ "Failed to resolve type hash to it's type") pure mTyp

  mKindHash <- replLookupTypesKind typeHash
  kindHash  <- maybe (replError . errCtx . EMsg . text $ "Failed to resolve type hash to it's kind hash") pure mKindHash

  mKind <- replLookupKind kindHash
  kind  <- maybe (replError . errCtx . EMsg . text $ "Failed to resolve kind hash to it's kind") pure mKind

  pure (expr,typ,kind)

-- | Lookup an expr by its Hash.
--
-- Hashes may be acquired from a prior replStoreExpr or found as a
-- ContentBinding inside an expression.
replLookupExpr
  :: Hash
  -> Repl (Maybe Expr)
replLookupExpr exprHash = do
  codeStore <- replCodeStore
  eRes <- replIO $ CodeStore.lookupExpr codeStore exprHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Failed to lookup expr") $ err

    Right (codeStore', expr)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure expr

-- | Lookup a type by its Hash.
--
-- Hashes may be acquired from a prior replStoreType.
replLookupType
  :: Hash
  -> Repl (Maybe Type)
replLookupType typeHash = do
  codeStore <- replCodeStore
  eRes <- replIO $ CodeStore.lookupType codeStore typeHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Failed to lookup type") $ err

    Right (codeStore', typ)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure typ

-- | Lookup a kind by its Hash.
--
-- Hashes may be acquired from a prior replStoreKind.
replLookupKind
  :: Hash
  -> Repl (Maybe Kind)
replLookupKind kindHash = do
  codeStore <- replCodeStore
  eRes <- replIO $ CodeStore.lookupKind codeStore kindHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Failed to lookup kind") $ err

    Right (codeStore', kind)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure kind

-- | Given an Expr Hash lookup the assocaited Type hash.
--
-- It is the callers responsibility to validate whether the association is true
-- if the backing storage is not reliable.
replLookupExprsType
  :: Hash
  -> Repl (Maybe Hash)
replLookupExprsType exprHash = do
  codeStore <- replCodeStore
  eRes <- replIO $ CodeStore.lookupExprType codeStore exprHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Failed to lookup exprs type") $ err

    Right (codeStore', typeHash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure typeHash

-- | Given an Expr Hash lookup the assocaited Type hash.
--
-- It is the callers responsibility to validate whether the association is true
-- if the backing storage is not reliable.
replLookupTypesKind
  :: Hash
  -> Repl (Maybe Hash)
replLookupTypesKind typeHash = do
  codeStore <- replCodeStore
  eRes <- replIO $ CodeStore.lookupTypeKind codeStore typeHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Failed to lookup types kind") $ err

    Right (codeStore', kindHash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure kindHash

-- | Attempt to resolve all ShortHashes contained within an expression (it's
-- patterns and it's types) into unambiguous ContentNames.
replResolveShortHashes
  :: ( ExprWithResolvedHashes unresolved resolved
     , AbstractionFor unresolved ~ TypeFor typePhase
     , AbstractionFor resolved   ~ TypeFor typePhase
     )
  => ExprFor unresolved
  -> Repl (ExprFor resolved)
replResolveShortHashes expr = do
  -- TODO: Settle on concrete phases at this level.
  codeStore <- replCodeStore
  let resolveCtx = mkResolveCtx codeStore
  eRes <- replIO . runResolve resolveCtx . resolveShortHashes $ expr
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Resolving short-hashes inside an expression") $ err

    Right (resolveCtx', resolvedExpr)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = _resolveCodeStore resolveCtx})
            pure resolvedExpr

-- | Attempt to resolve a ShortHash for an expression to an unambiguous Hash.
--
-- It is an error if:
-- - There is no known larger Hash
-- - There are multiple colliding Hashes
replResolveExprHash
  :: ShortHash
  -> Repl Hash
replResolveExprHash shortHash = do
  codeStore <- replCodeStore
  let resolveCtx = mkResolveCtx codeStore
  eRes <- replIO . runResolve resolveCtx . resolveExprHash $ shortHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Resolving a short-hash to an expression hash") $ err

    Right (resolveCtx', resolvedHash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = _resolveCodeStore resolveCtx})
            pure resolvedHash

-- | Attempt to resolve a ShortHash for a type to an unambiguous Hash.
--
-- It is an error if:
-- - There is no known larger Hash
-- - There are multiple colliding Hashes
replResolveTypeHash
  :: ShortHash
  -> Repl Hash
replResolveTypeHash shortHash = do
  codeStore <- replCodeStore
  let resolveCtx = mkResolveCtx codeStore
  eRes <- replIO . runResolve resolveCtx . resolveTypeHash $ shortHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Resolving a short-hash to a type hash") $ err

    Right (resolveCtx', resolvedHash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = _resolveCodeStore resolveCtx})
            pure resolvedHash

-- | Attempt to resolve a ShortHash for a kind to an unambiguous Hash.
--
-- It is an error if:
-- - There is no known larger Hash
-- - There are multiple colliding Hashes
replResolveKindHash
  :: ShortHash
  -> Repl Hash
replResolveKindHash shortHash = do
  codeStore <- replCodeStore
  let resolveCtx = mkResolveCtx codeStore
  eRes <- replIO . runResolve resolveCtx . resolveKindHash $ shortHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Resolving a short-hash to a kind hash") $ err

    Right (resolveCtx', resolvedHash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = _resolveCodeStore resolveCtx})
            pure resolvedHash


-- | Attempt to shorten all Hashes contained within an expression (it's patterns
-- and it's types) into the shortest unambiguous ShortHashes.
replShortenHashes
  :: ( ExprWithShortenedHashes long short
     , AbstractionFor long ~ TypeFor typePhase
     , AbstractionFor short ~ TypeFor typePhase
     )
  => ExprFor long
  -> Repl (ExprFor short)
replShortenHashes expr = do
  codeStore <- replCodeStore
  let resolveCtx = mkResolveCtx codeStore
  eRes <- replIO . runResolve resolveCtx . shortenHashes $ expr
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Shortening hashes within an expression") $ err

    Right (resolveCtx', shortenedExpr)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = _resolveCodeStore resolveCtx})
            pure shortenedExpr

-- | Given an Expr Hash, shorten it to the shortest unambiguous ShortHash.
replShortenExprHash
  :: Hash
  -> Repl ShortHash
replShortenExprHash exprHash = do
  codeStore <- replCodeStore
  let resolveCtx = mkResolveCtx codeStore
  eRes <- replIO . runResolve resolveCtx . shortenExprHash $ exprHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Shortening an expression hash") $ err

    Right (resolveCtx', shortHash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = _resolveCodeStore resolveCtx})
            pure shortHash

-- | Given an Type Hash, shorten it to the shortest unambiguous ShortHash.
replShortenTypeHash
  :: Hash
  -> Repl ShortHash
replShortenTypeHash typeHash = do
  codeStore <- replCodeStore
  let resolveCtx = mkResolveCtx codeStore
  eRes <- replIO . runResolve resolveCtx . shortenTypeHash $ typeHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Shortening a type hash") $ err

    Right (resolveCtx', shortHash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = _resolveCodeStore resolveCtx})
            pure shortHash

-- | Given an Kind Hash, shorten it to the shortest unambiguous ShortHash.
replShortenKindHash
  :: Hash
  -> Repl ShortHash
replShortenKindHash kindHash = do
  codeStore <- replCodeStore
  let resolveCtx = mkResolveCtx codeStore
  eRes <- replIO . runResolve resolveCtx . shortenKindHash $ kindHash
  case eRes of
    Left err
      -> replError . EContext (EMsg $ text "Shortening a kind hash") $ err

    Right (resolveCtx', shortHash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = _resolveCodeStore resolveCtx})
            pure shortHash

{-  Evaluation -}

-- | Evaluating a top-level expression (which is expected to be reduced and
-- type-checked) by substituting ContentBindings from the CodeStore, binding
-- variables and reducing until the expression no longer changes.
--
-- Unlike reduction, evaluation:
-- - Does not evaluate under abstractions (such as lambdas and type-lambdas)
-- - _Does_ evaluate under ContentBindings (which must be available in the
-- CodeStore).
--
-- Evaluate should eventually terminate given enough gas as there are currently
-- no recursive references allowed in the AST, nor are there self-references.
replEvaluateExpr
  :: Expr
  -> Maybe Int -- ^ Optional gas limit. When reached, evaluation will halt.
  -> Repl (EvaluationCtx, Expr)
replEvaluateExpr expr gas = do
  typeCtx   <- replTypeCtx
  codeStore <- replCodeStore
  let evaluationCtx = (topEvaluationCtx typeCtx codeStore){ _evaluationGas = gas }

  eRes <- replIO . runEvaluate evaluationCtx . evaluate $ expr
  case eRes of
    Left err
      -> replError $ EContext (EMsg $ text "Evaluating top-level expression") $ err

    Right (evaluationCtx, reducedExpr)
      -> pure (evaluationCtx, reducedExpr)

-- | Evaluating a top-level type (which is expected to be reduced and
-- kind-checked) by binding variables and reducing until the type no longer
-- changes.
--
-- This is currently identical to reduceType.
replEvaluateType
  :: Type
  -> Maybe Int
  -> Repl (EvaluationCtx, Type)
replEvaluateType typ gas = do
  typeCtx   <- replTypeCtx
  codeStore <- replCodeStore
  let evaluationCtx = (topEvaluationCtx typeCtx codeStore){ _evaluationGas = gas }

  eRes <- replIO . runEvaluate evaluationCtx . evaluateType $ typ
  case eRes of
    Left err
      -> replError $ EContext (EMsg $ text "Evaluating top-level type") $ err

    Right (evaluationCtx, reducedType)
      -> pure (evaluationCtx, reducedType)

