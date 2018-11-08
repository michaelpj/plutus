{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Plutus.CoreToPLC.Compiler.Definitions where

import           Language.Plutus.CoreToPLC.Error
import           Language.Plutus.CoreToPLC.PIRTypes
import           Language.Plutus.CoreToPLC.Compiler.Types

import qualified Language.PlutusIR                as PIR
import qualified Language.PlutusIR.MkPir          as PIR

import qualified GhcPlugins                         as GHC

import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.Graph                         as Graph
import qualified Data.Map                           as Map
import qualified Data.Set                           as Set

import Lens.Micro

defineType :: Converting m => GHC.Name -> PIRBinding -> [GHC.Name] -> m ()
defineType name def deps = modify $ over typeDefs (Map.insert name (def, deps))

lookupTypeDef :: Converting m => GHC.Name -> m (Maybe PIRType)
lookupTypeDef name = do
    ConvertingState{csTypeDefs=defs, csAliases=as} <- get
    case Map.lookup name defs of
        Just (td, _) -> do
            ty <- if Set.member name as then defRealTy td else defTy td
            pure $ Just ty
        Nothing -> pure Nothing

defineTerm :: Converting m => GHC.Name -> PIRBinding -> [GHC.Name] -> m ()
defineTerm name def deps = modify $ over termDefs (Map.insert name (def, deps))

recordAlias :: Converting m => GHC.Name -> m ()
recordAlias name = modify $ over aliases (Set.insert name)

lookupTermDef :: Converting m => GHC.Name -> m (Maybe PIRTerm)
lookupTermDef name = do
    defs <- gets csTermDefs
    case Map.lookup name defs of
        Just (td, _) -> Just <$> defTerm td
        Nothing -> pure Nothing

defRealTy :: Converting m => PIRBinding -> m PIRType
defRealTy = \case
    PIR.TermBind () _ _ -> throwPlain $ ConversionError "Asked for real type from term binding"
    PIR.TypeBind () _ ty -> pure ty
    PIR.DatatypeBind () (PIR.Datatype () _ _ _ _) -> throwPlain $ ConversionError "Asked for real type from term binding"

defTy :: Converting m => PIRBinding -> m PIRType
defTy = \case
    PIR.TermBind () _ _ -> throwPlain $ ConversionError "Asked for type from term binding"
    PIR.TypeBind () n _ -> pure $ PIR.mkTyVar () n
    PIR.DatatypeBind () (PIR.Datatype () n _ _ _) -> pure $ PIR.mkTyVar () n

defConstrs :: Converting m => PIRBinding -> m (Maybe [PIRTerm])
defConstrs = \case
    PIR.TermBind () _ _ -> throwPlain $ ConversionError "Asked for constructors from term binding"
    PIR.TypeBind () _ _ -> pure Nothing
    PIR.DatatypeBind () (PIR.Datatype () _ _ _ constrs) -> pure $ Just $ fmap (PIR.mkVar ()) constrs

defMatch :: Converting m => PIRBinding -> m (Maybe PIRTerm)
defMatch = \case
    PIR.TermBind () _ _ -> throwPlain $ ConversionError "Asked for matcher from term binding"
    PIR.TypeBind () _ _ -> pure Nothing
    PIR.DatatypeBind () (PIR.Datatype () _ _ destr _) -> pure $ Just $ PIR.Var () destr

defTerm :: Converting m => PIRBinding -> m PIRTerm
defTerm = \case
    PIR.TermBind () n _ -> pure $ PIR.mkVar () n
    PIR.TypeBind () _ _ -> throwPlain $ ConversionError "Asked for term from type binding"
    PIR.DatatypeBind () _ -> throwPlain $ ConversionError "Asked for term from datatype binding"

-- Processing definitions

-- | Given the definitions in the program, create a topologically ordered list of the SCCs using the dependency information
defSccs :: DefMap GHC.Name PIRBinding -> DefMap GHC.Name PIRBinding -> [Graph.SCC PIRBinding]
defSccs typeDefs termDefs =
    let
        typeInputs = fmap (\(ghcName, (d, deps)) -> (d, ghcName, deps)) (Map.assocs typeDefs)
        termInputs = fmap (\(ghcName, (d, deps)) -> (d, ghcName, deps)) (Map.assocs termDefs)
    in
        Graph.stronglyConnComp (typeInputs ++ termInputs)

wrapWithDefs
    :: (MonadError ConvError m)
    => DefMap GHC.Name PIRBinding
    -> DefMap GHC.Name PIRBinding
    -> PIRTerm
    -> m PIRTerm
wrapWithDefs typeDefs termDefs body = do
    let sccs = defSccs typeDefs termDefs
    -- process from the inside out
    foldM wrapDefScc body (reverse sccs)

wrapDefScc
    :: (MonadError ConvError m)
    => PIRTerm
    -> Graph.SCC PIRBinding
    -> m PIRTerm
wrapDefScc acc scc = case scc of
    Graph.AcyclicSCC def -> pure $ PIR.Let () PIR.NonRec [ def ] acc
    Graph.CyclicSCC defs -> pure $ PIR.Let () PIR.Rec defs acc
