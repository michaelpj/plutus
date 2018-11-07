{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-unused-do-bind #-}
{-# OPTIONS -fplugin=Language.Plutus.CoreToPLC.Plugin -fplugin-opt Language.Plutus.CoreToPLC.Plugin:dont-typecheck #-}
module Spec.Marlowe(tests) where

import           Data.Bifunctor                                      (Bifunctor (..))
import           Data.Either                                         (isRight)
import           Data.Foldable                                       (traverse_)
import qualified Data.Map                                            as Map
import           Hedgehog                                            (Property, forAll, property)
import qualified Hedgehog
import           Test.Tasty
import           Test.Tasty.Hedgehog                                 (testProperty)

import           Wallet.API                                          (PubKey (..))
import           Wallet.Emulator                                     hiding (Value)
import qualified Wallet.Generators                                   as Gen

import qualified Language.Plutus.Runtime                             as Runtime
import           Language.Plutus.TH
                          (plutus)
import           Language.Marlowe.Compiler
import qualified Wallet.UTXO                                         as UTXO
import qualified Debug.Trace as Debug

tests :: TestTree
tests = testGroup "Marlowe" [
        testProperty "Marlowe" marlowe
        ]

-- | Funds available to wallets `Wallet 2` and `Wallet 3`
startingBalance :: UTXO.Value
startingBalance = 1000

marlowe :: Property
marlowe = property $ do
    let model = Gen.generatorModel { Gen.gmInitialBalance = Map.singleton (PubKey 1) 1000 }
    (result, st) <- forAll $ Gen.runTraceOn model $ do
        -- Init a contract
        let w = Wallet 1
            update = blockchainActions >>= walletsNotifyBlock [w]
        update
        txs <- walletAction w (createContract Null 1)
        update
        let (txOut, txOutRef) = head . filter (isPayToScriptOut . fst) . txOutRefs $ head txs
        -- Debug.traceM $ show txOutRef
        txs1 <- walletAction w (endContract Null txOutRef 1)
        update
        Debug.traceM $ show txs1
        -- walletAction w ()
        return ()
    Hedgehog.assert (True)

