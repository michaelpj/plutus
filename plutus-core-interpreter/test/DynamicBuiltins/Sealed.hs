module DynamicBuiltins.Sealed
    ( test_sealUnseal
    ) where

import           Language.PlutusCore
import           Language.PlutusCore.Constant
import           Language.PlutusCore.Constant.Dynamic
import           Language.PlutusCore.MkPlc

import           Language.PlutusCore.StdLib.Data.List        as Plc
import           Language.PlutusCore.StdLib.Data.Unit

import           DynamicBuiltins.Common

import           Control.Monad.Except
import           Data.Either                                 (isRight)
import           Data.Proxy
import           Test.Tasty
import           Test.Tasty.HUnit

test_sealUnseal :: TestTree
test_sealUnseal = testCase "sealUnseal" $ do
    let
        integer = TyBuiltin () TyInteger
        term
            -- = Apply () (TyInst () (dynamicBuiltinNameAsTerm dynamicUnsealName) integer)
            = Apply () (TyInst () (dynamicBuiltinNameAsTerm dynamicSealName) integer)
            $ Constant () (BuiltinInt () 1)
        env = insertDynamicBuiltinNameDefinition dynamicSealDefinition $
              insertDynamicBuiltinNameDefinition dynamicUnsealDefinition mempty
        run = typecheckReadKnownCek env term
    case run of
        Left e  -> assertFailure $ "Should typecheck: " <> show e
        Right t -> t @=? Right (EvaluationSuccess (1::Integer))
