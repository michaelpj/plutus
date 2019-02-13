{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
-- | Optimization passes for removing dead code, mainly dead let bindings.
module Language.PlutusIR.Optimizer.DeadCode (removeDeadBindings) where

import           Language.PlutusIR
import qualified Language.PlutusIR.Analysis.Dependencies as Deps
import           Language.PlutusIR.MkPir

import qualified Language.PlutusCore                     as PLC
import qualified Language.PlutusCore.Name                as PLC

import           Control.Lens

import           Data.Coerce
import qualified Data.Set                                as Set

import qualified Algebra.Graph                           as G
import qualified Algebra.Graph.AdjacencyMap              as AM
import qualified Algebra.Graph.ToGraph                   as T

-- | Remove all the dead let bindings in a term.
removeDeadBindings
    :: (PLC.HasUnique (name a) PLC.TermUnique, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Term tyname name a
    -> Term tyname name a
removeDeadBindings t = rewriteOf subTerms (processTerm (computeLiveness t)) t

type Liveness = Set.Set Deps.Node

computeLiveness
    :: (PLC.HasUnique (name a) PLC.TermUnique, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Term tyname name a
    -> Liveness
computeLiveness t =
    let
        depGraph :: G.Graph Deps.Node
        depGraph = Deps.runTermDeps t
    in Set.fromList $ AM.reachable Deps.Root (T.toAdjacencyMap depGraph)

live :: (PLC.HasUnique n unique) => Liveness -> n -> Bool
live l n =
    let u = coerce $ n ^. PLC.unique
    in Set.member (Deps.Variable u) l

liveBinding
    :: (PLC.HasUnique (name a) PLC.TermUnique, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Liveness
    -> Binding tyname name a
    -> Bool
liveBinding l b =
    let
        -- TODO: HasUnique instances for VarDecl and TyVarDecl?
        liveVarDecl (VarDecl _ n _) = live l n
        liveTyVarDecl (TyVarDecl _ n _) = live l n
    in case b of
        TermBind _ d _ -> liveVarDecl d
        TypeBind _ d _ -> liveTyVarDecl d
        DatatypeBind _ (Datatype _ d _ destr constrs) -> or ([liveTyVarDecl d,  live l destr] ++ fmap liveVarDecl constrs)

processTerm
    :: (PLC.HasUnique (name a) PLC.TermUnique, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Liveness
    -> Term tyname name a
    -> Maybe (Term tyname name a)
processTerm l = \case
    Let x r bs t ->
        -- throw away dead bindings
        let liveBindings = filter (liveBinding l) bs
        -- important: return Nothing if we don't want to transform otherwise using rewriteOf won't terminate
        in if length bs == length liveBindings then Nothing else Just $ mkLet x r liveBindings t
    _ -> Nothing
