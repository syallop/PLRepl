{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
  -- Ironically these function names themselves are bad.
  -- This is because the types arent specific enough to rule out misuse so care
  -- needs to be taken choosing the right function for the expected sort of
  -- contentname.
  , replGatherExprsExprContentNames
  , replGatherExprsTypeContentNames
  , replGatherTypesTypeContentNames

  , replResolveExprContentTypeHashes
  , replResolveTypeContentKindHashes

  , replResolveExprsExprContentTypes
  , replResolveTypesTypeContentKinds
  , replResolveExprsTypeContentTypes

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
import PL.CodeStore
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
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCheck
import PL.TypeCtx
import qualified PL.CodeStore as CodeStore

import qualified PLParser as PLParser
import PLPrinter
import PLGrammar

import Control.Monad
import Data.Foldable
import Data.List (intercalate,intersperse)
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
  fmap Map.fromList . mapM (\h -> do ty <- replLookupExprsType . contentName $ h
                                     pure (h,ty)
                           )
                    . Set.toList

-- Fails if any name does not resolve.
replResolveTypeContentTypes
  :: Set ContentName
  -> Repl (Map ContentName Type)
replResolveTypeContentTypes =
  fmap Map.fromList . mapM (\h -> do ty <- replLookupType . contentName $ h
                                     pure (h,ty)
                           )
                    . Set.toList

-- | Resolve all type ContentNames to their kind hash.
--
-- Fails if any name does not resolve.
replResolveTypeContentKindHashes
  :: Set ContentName
  -> Repl (Map ContentName Hash)
replResolveTypeContentKindHashes =
  fmap Map.fromList . mapM (\h -> do ty <- replLookupTypesKind . contentName $ h
                                     pure (h,ty)
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
replResolveExprsExprContentTypes expr =
  replGatherExprsExprContentNames expr
    >>= replResolveExprContentTypeHashes
    >>= mapM replLookupType

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
    >>= mapM replLookupKind

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
  mRes <- replIO $ storeExpr codeStore expr
  case mRes of
    Nothing
      -> replError $ EMsg $ text "Failed to store expression"

    Just (codeStore', storeResult, hash)
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
  mRes <- replIO $ storeType codeStore typ
  case mRes of
    Nothing
      -> replError $ EMsg $ text "Failed to store type"

    Just (codeStore', storeResult, hash)
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
  mRes <- replIO $ storeKind codeStore kind
  case mRes of
    Nothing
      -> replError $ EMsg $ text "Failed to store kind"

    Just (codeStore', storeResult, hash)
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
  mRes <- replIO $ storeExprHasType codeStore (exprHash, typeHash)
  case mRes of
    Nothing
      -> replError $ EMsg $ text "Failed to store expression-has-type relation"

    Just (codeStore', storeResult)
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
  mRes <- replIO $ storeTypeHasKind codeStore (typeHash, kindHash)
  case mRes of
    Nothing
      -> replError $ EMsg $ text "Failed to store type-has-kind relation"

    Just (codeStore', storeResult)
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
  expr     <- replLookupExpr      exprHash
  typeHash <- replLookupExprsType exprHash
  typ      <- replLookupType      typeHash
  kindHash <- replLookupTypesKind typeHash
  kind     <- replLookupKind      kindHash
  pure (expr,typ,kind)

-- | Lookup an expr by its Hash.
--
-- Hashes may be acquired from a prior replStoreExpr or found as a
-- ContentBinding inside an expression.
replLookupExpr
  :: Hash
  -> Repl Expr
replLookupExpr exprHash = do
  codeStore <- replCodeStore
  mRes <- replIO $ lookupExpr codeStore exprHash
  case mRes of
    Nothing
      -> replError $ EMsg $ text "Failed to lookup expr"

    Just (codeStore', expr)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure expr

-- | Lookup a type by its Hash.
--
-- Hashes may be acquired from a prior replStoreType.
replLookupType
  :: Hash
  -> Repl Type
replLookupType typeHash = do
  codeStore <- replCodeStore
  mRes <- replIO $ lookupType codeStore typeHash
  case mRes of
    Nothing
      -> replError $ EMsg $ text "Failed to lookup type"

    Just (codeStore', typ)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure typ

-- | Lookup a kind by its Hash.
--
-- Hashes may be acquired from a prior replStoreKind.
replLookupKind
  :: Hash
  -> Repl Kind
replLookupKind kindHash = do
  codeStore <- replCodeStore
  mRes <- replIO $ lookupKind codeStore kindHash
  case mRes of
    Nothing
      -> replError $ EMsg $ text "Failed to lookup kind"

    Just (codeStore', kind)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure kind

-- | Given an Expr Hash lookup the assocaited Type hash.
--
-- It is the callers responsibility to validate whether the association is true
-- if the backing storage is not reliable.
replLookupExprsType
  :: Hash
  -> Repl Hash
replLookupExprsType exprHash = do
  codeStore <- replCodeStore
  mRes <- replIO $ lookupExprType codeStore exprHash
  case mRes of
    Nothing
      -> replError $ EMsg $ text "Failed to lookup exprs type"

    Just (codeStore', typeHash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure typeHash

-- | Given an Expr Hash lookup the assocaited Type hash.
--
-- It is the callers responsibility to validate whether the association is true
-- if the backing storage is not reliable.
replLookupTypesKind
  :: Hash
  -> Repl Hash
replLookupTypesKind typeHash = do
  codeStore <- replCodeStore
  mRes <- replIO $ lookupTypeKind codeStore typeHash
  case mRes of
    Nothing
      -> replError $ EMsg $ text "Failed to lookup types kind"

    Just (codeStore', kindHash)
      -> do replModifyCtx (\ctx -> ctx{_replCodeStore = codeStore'})
            pure kindHash

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

