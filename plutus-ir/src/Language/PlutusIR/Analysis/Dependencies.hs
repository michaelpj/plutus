{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
-- | Functions for computing the dependency graph of variables within a term or type.
module Language.PlutusIR.Analysis.Dependencies (Node (..), HK.Graph, runTermDeps, runTypeDeps) where

import qualified Language.PlutusCore               as PLC
import qualified Language.PlutusCore.Name          as PLC

import           Language.PlutusIR
import qualified Language.PlutusIR.Analysis.Usages as Usages

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader

import           Data.Foldable

import qualified Algebra.Graph.HigherKinded.Class  as HK
import qualified Data.Set                          as Set

-- | A node in a dependency graph. Either a specific 'PLC.Unique', or a specific
-- node indicating the root of the graph. We need the root node because when computing the
-- dependency graph of, say, a term, there will not be a binding for the term itself which
-- we can use to represent it in the graph.
data Node = Variable PLC.Unique | Root deriving (Show, Eq, Ord)

-- | Compute the dependency graph of a 'Term'. The 'Root' node will correspond to the term itself.
--
-- For example, the graph of @[(let (nonrec) (vardecl x t) y) [x z]]@ is
-- @
--     ROOT -> x
--     ROOT -> z
--     x -> y
--     x -> t
-- @
runTermDeps
    :: (HK.Graph g, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => Term tyname name a
    -> g Node
runTermDeps t = runReader (termDeps t) Root

-- | Compute the dependency graph of a 'Type'. The 'Root' node will correspond to the type itself.
--
-- This graph will always be simply a star from 'Root', since types have no internal let-bindings.
--
-- For example, the graph of @(all a (type) [f a])@ is:
-- @
--     ROOT -> f
--     ROOT -> a
-- @
--
runTypeDeps
    :: (HK.Graph g, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Type tyname a
    -> g Node
runTypeDeps t = runReader (typeDeps t) Root

-- | Record some dependencies on the current node.
recordDeps
    :: (HK.Graph g, MonadReader Node m)
    => [PLC.Unique]
    -> m (g Node)
recordDeps us = do
    current <- ask
    let currentVertex = pure current
    let targetVertices = fmap (pure . Variable) us
    pure $ HK.connect currentVertex (asum targetVertices)

-- | Process the given action with the given name as the current node.
withCurrent
    :: (MonadReader Node m, PLC.HasUnique n u)
    => n
    -> m g
    -> m g
withCurrent n = local (const $ Variable $ n ^. PLC.unique . coerced)

bindingDeps
    :: (HK.Graph g, MonadReader Node m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => Binding tyname name a
    -> m (g Node)
bindingDeps b = case b of
    TermBind _ d@(VarDecl _ n _) rhs -> do
        vDeps <- varDeclDeps d
        tDeps <- withCurrent n $ termDeps rhs
        pure $ vDeps <|> tDeps
    TypeBind _ d@(TyVarDecl _ n _) rhs -> do
        vDeps <- tyVarDeclDeps d
        tDeps <- withCurrent n $ typeDeps rhs
        pure $ vDeps <|> tDeps
    DatatypeBind _ (Datatype _ d tvs _ constrs) -> do
        vDeps <- tyVarDeclDeps d
        tvDeps <- traverse tyVarDeclDeps tvs
        cstrDeps <- traverse varDeclDeps constrs
        pure $ asum $ [vDeps] ++ tvDeps ++ cstrDeps

varDeclDeps
    :: (HK.Graph g, MonadReader Node m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => VarDecl tyname name a
    -> m (g Node)
varDeclDeps (VarDecl _ n ty) = withCurrent n $ typeDeps ty

-- Here for completeness, but doesn't do much
tyVarDeclDeps
    :: (HK.Graph g, MonadReader Node m)
    => TyVarDecl tyname a
    -> m (g Node)
tyVarDeclDeps _ = pure empty

-- | Compute the dependency graph of a term. Takes an initial 'Node' indicating what the term itself depends on
-- (usually 'Root' if it is the real term you are interested in).
termDeps
    :: (HK.Graph g, MonadReader Node m, PLC.HasUnique (tyname a) PLC.TypeUnique, PLC.HasUnique (name a) PLC.TermUnique)
    => Term tyname name a
    -> m (g Node)
termDeps = \case
    Let _ _ bs t -> do
        bGraphs <- traverse bindingDeps bs
        bodyGraph <- termDeps t
        pure $ asum $ bGraphs ++ [bodyGraph]
    Var _ n -> recordDeps [n ^. PLC.unique . coerced]
    TyAbs _ _ _ t -> termDeps t
    LamAbs _ _ ty t -> (<|>) <$> typeDeps ty <*> termDeps t
    Apply _ t1 t2 -> (<|>) <$> termDeps t1 <*> termDeps t2
    TyInst _ t ty -> (<|>) <$> termDeps t <*> typeDeps ty
    Error _ ty -> typeDeps ty
    Wrap _ _ ty t -> (<|>) <$> typeDeps ty <*> termDeps t
    Unwrap _ t -> termDeps t
    Constant{} -> pure empty
    Builtin{} -> pure empty

-- | Compute the dependency graph of a type. Takes an initial 'Node' indicating what the type itself depends on
-- (usually 'Root' if it is the real type you are interested in).
typeDeps
    :: (HK.Graph g, MonadReader Node m, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Type tyname a
    -> m (g Node)
typeDeps ty =
    -- The dependency graph of a type is very simple since it doesn't have any internal let-bindings. So we just
    -- need to find all the used variables and mark them as dependencies of the current node.
    let
        used = Usages.allUsed $ Usages.runTypeUsages ty
    in recordDeps (Set.toList used)
