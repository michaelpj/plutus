{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
module Spec.MultiSigStateMachine(tests) where

import           Control.Monad                                                 (foldM, foldM_, void, (>=>))
import           Data.Either                                                   (isLeft, isRight)
import           Data.Foldable                                                 (traverse_)
import qualified Data.Map                                                      as Map
import qualified Data.Text                                                     as T
import           Test.Tasty                                                    (TestTree, testGroup)
import qualified Test.Tasty.HUnit                                              as HUnit

import           Spec.Lib                                                      as Lib

import qualified Ledger.Ada                                                    as Ada
import qualified Ledger.Typed.Scripts                                          as Scripts
import           Ledger.Value                                                  (Value, scale)
import           Wallet.API                                                    (WalletAPI,
                                                                                WalletDiagnostics)
import qualified Wallet.Emulator                                               as EM

import qualified Language.PlutusTx as PlutusTx

import           Language.Plutus.Contract.Test
import qualified Language.Plutus.Contract.Trace                  as Trace
import qualified Language.Plutus.Contract.StateMachine                  as SM
import qualified Language.PlutusTx.StateMachine                  as SM

import           Language.PlutusTx.Coordination.Contracts.MultiSigStateMachine (Payment, State)
import qualified Language.PlutusTx.Coordination.Contracts.MultiSigStateMachine as MS

type MSTrace m a = ContractTrace (SM.SMSchema MS.State MS.Input) MS.Error m a

tests :: TestTree
tests = testGroup "multi sig state machine tests" [
    checkPredicate "Expose 'initalise' endpoint"
        (MS.contract machine)
        (endpointAvailable @"initialise" w1)
        $ pure ()
    ,
    checkPredicate "lock, propose, sign x3, pay"
        (MS.contract machine)
        --(endpointAvailable @"step" w1)
        (assertDone w1 (const False) "")
        --(walletFundsChange w2 (MS.paymentAmount payment))
        $ lockProposeSignPay 3 1
    ,
    --HUnit.testCaseSteps "lock, propose, sign 3x, pay - SUCCESS" (runTrace (lockProposeSignPay 3 1) isRight),
    --HUnit.testCaseSteps "lock, propose, sign 2x, pay - FAILURE" (runTrace (lockProposeSignPay 2 1) isLeft),
    --HUnit.testCaseSteps "lock, propose, sign 3x, pay x2 - SUCCESS" (runTrace (lockProposeSignPay 3 2) isRight),
    --HUnit.testCaseSteps "lock, propose, sign 3x, pay x3 - FAILURE" (runTrace (lockProposeSignPay 3 3) isLeft),
    Lib.goldenPir "test/Spec/multisigStateMachine.pir" $$(PlutusTx.compile [|| MS.mkValidator ||]),
    HUnit.testCase "script size is reasonable" (Lib.reasonable (Scripts.validatorScript $ MS.scriptInstance params) 350000)
    ]

{-
runTrace :: EM.EmulatorAction a -> (Either EM.AssertionError a -> Bool) -> (String -> IO ()) -> IO ()
runTrace t f step = do
    let initialState = EM.emulatorStateInitialDist (Map.singleton (EM.walletPubKey (EM.Wallet 1)) (Ada.adaValueOf 10))
        (result, st) = EM.runEmulator initialState t
    if f result
    then pure ()
    else do
        step (show $ EM.emLog st)
        HUnit.assertFailure "transaction failed to validate"
-}

processAndNotify :: EM.Trace m ()
processAndNotify = void (EM.addBlocksAndNotify [w1, w2, w3] 1)

w1, w2, w3 :: EM.Wallet
w1 = EM.Wallet 1
w2 = EM.Wallet 2
w3 = EM.Wallet 3

-- | A multisig contract that requires 3 out of 5 signatures
params :: MS.Params
params = MS.Params keys 3 where
    keys = EM.walletPubKey . EM.Wallet <$> [1..5]

machine :: SM.StateMachineInstance MS.State MS.Input
machine = MS.machineInstance params

-- | A payment of 5 Ada to the public key address of wallet 2
payment :: MS.Payment
payment =
    MS.Payment
        { MS.paymentAmount    = Ada.lovelaceValueOf 5
        , MS.paymentRecipient = EM.walletPubKey w2
        , MS.paymentDeadline  = 20
        }

lock' :: (MonadEmulator MS.Error m) => Value -> MSTrace m a ()
-- wallet 1 locks the funds
lock' value = Trace.callEndpoint @"initialise" w1 (MS.Holding value, value) >> Trace.handleBlockchainEvents w1

proposePayment' :: (MonadEmulator MS.Error m) => MSTrace m a ()
proposePayment' = Trace.callEndpoint @"step" w1 (MS.ProposePayment payment) >> Trace.handleBlockchainEvents w1

addSignature' :: (MonadEmulator MS.Error m) => Integer -> MSTrace m a ()
-- i wallets add their signatures
addSignature' i = mapM_ (\w -> Trace.callEndpoint @"step" w (MS.AddSignature (Trace.walletPubKey w)) >> Trace.handleBlockchainEvents w) (take (fromIntegral i) [w1, w2, w3])

makePayment' :: (MonadEmulator MS.Error m) => MSTrace m a ()
makePayment' = Trace.callEndpoint @"step" w1 MS.Pay >> Trace.handleBlockchainEvents w1 -- >> Trace.handleBlockchainEvents w1

proposeSignPay :: (MonadEmulator MS.Error m) => Integer -> MSTrace m a ()
proposeSignPay i = proposePayment' >> addSignature' i >> makePayment'

lockProposeSignPay :: (EM.MonadEmulator MS.Error m) => Integer -> Integer -> MSTrace m a ()
lockProposeSignPay i j = do
    lock' (Ada.lovelaceValueOf 10)
    proposePayment'
    addSignature' 1
    makePayment'
    --EM.assertOwnFundsEq w2 (scale j (Ada.adaValueOf 5))

{-
lockProposeSignPay :: (EM.MonadEmulator m) => Integer -> Integer -> MSTrace m a ()
lockProposeSignPay i j = EM.processEmulated $ do

    -- stX contain the state of the contract. See note [Current state of the
    -- contract] in
    -- Language.PlutusTx.Coordination.Contracts.MultiSigStateMachine
    initialise''
    st1 <- lock'' (Ada.adaValueOf 10)

    foldM_ (\st _ -> proposeSignPay i st) st1 [1..j]

    processAndNotify
    EM.assertOwnFundsEq w2 (scale j (Ada.adaValueOf 5))

lockTrace
    :: ( MonadEmulator m )
    => ContractTrace GameSchema m a ()
lockTrace =
    let w1 = Trace.Wallet 1
        w2 = Trace.Wallet 2 in
    Trace.callEndpoint @"lock" w1 (LockParams "secret" 10)
        >> Trace.notifyInterestingAddresses w2
        >> Trace.handleBlockchainEvents w1
-}
