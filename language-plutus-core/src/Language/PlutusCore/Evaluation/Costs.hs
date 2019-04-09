{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusCore.Evaluation.Costs where

import           Language.PlutusCore

import           Control.Lens

data CostModel arg = CostModel {
    _costModelApplication     :: Gas
    , _costModelInstantiation :: Gas
    , _costModelBuiltin       :: BuiltinName -> [arg] -> Gas
    }

makeLenses ''CostModel

defaultCostModel :: CostModel (Term TyName Name a)
defaultCostModel = CostModel {
    _costModelApplication = Gas 1
    , _costModelInstantiation = Gas 0
    , _costModelBuiltin = defaultBuiltinCostFunction
  }

defaultBuiltinCostFunction :: BuiltinName -> [Term TyName Name a] -> Gas
defaultBuiltinCostFunction n args = case n of
    AddInteger           -> Gas 1
    SubtractInteger      -> Gas 1
    MultiplyInteger      -> Gas 1
    DivideInteger        -> Gas 1
    QuotientInteger      -> Gas 1
    ModInteger           -> Gas 1
    RemainderInteger     -> Gas 1
    LessThanInteger      -> Gas 1
    LessThanEqInteger    -> Gas 1
    GreaterThanInteger   -> Gas 1
    GreaterThanEqInteger -> Gas 1
    EqInteger            -> Gas 1
    Concatenate          -> Gas 1
    TakeByteString       -> Gas 1
    DropByteString       -> Gas 1
    EqByteString         -> Gas 1
    SHA2                 -> Gas 1
    SHA3                 -> Gas 1
    VerifySignature      -> Gas 1
