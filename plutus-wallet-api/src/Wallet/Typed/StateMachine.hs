{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module Wallet.Typed.StateMachine where

import           Control.Monad.Except

import           Control.Lens
import           Control.Monad.Error.Lens

import           Data.Maybe
import qualified Data.Map                       as Map

import qualified Language.PlutusTx              as PlutusTx
import qualified Language.PlutusTx.StateMachine as SM
import qualified Ledger.Typed.Tx                as Typed
import           Ledger.AddressMap
import           Ledger.Value
import qualified Ledger.Typed.Scripts           as Scripts
import qualified Ledger                         as Ledger

data SMError s i = InvalidTransition s i
makeClassyPrisms ''SMError

instance (Show s, Show i) => Show (SMError s i) where
    show (InvalidTransition s i) = "Invalid transition from\n" ++ show s ++ "\nwith\n" ++ show i

type OnChainState s i = (Typed.TypedScriptTxOut (SM.StateMachine s i), Typed.TypedScriptTxOutRef (SM.StateMachine s i), s)
type SteppingTx s i = Typed.TypedTx '[SM.StateMachine s i] '[SM.StateMachine s i]
type HaltingTx s i = Typed.TypedTx '[SM.StateMachine s i] '[]

getStates
    :: forall s i
    . (PlutusTx.IsData s)
    => SM.StateMachineInstance s i
    -> AddressMap
    -> [OnChainState s i]
getStates (SM.StateMachineInstance _ si) am =
    let refMap = fromMaybe Map.empty $ am ^. at (Scripts.scriptAddress si)
    in flip mapMaybe (Map.toList refMap) $ \(ref, out) -> do
        tref <- either (const Nothing) pure $ Typed.typeScriptTxOutRef (\r -> Map.lookup r refMap) si ref
        tout <- either (const Nothing) pure $ Typed.typeScriptTxOut si out
        s <- Typed.getData tout
        pure (tout, tref, s)

mkInitialise
    :: forall s i
    . (PlutusTx.IsData s)
    => SM.StateMachineInstance s i
    -- ^ The parameters of the contract instance
    -> s
    -- ^ Initial state.
    -> Value
    -- ^ The funds we want to lock.
    -> (Typed.TypedTx '[] '[SM.StateMachine s i], s)
    -- ^ The initializing transaction and the initial state of the contract.
mkInitialise (SM.StateMachineInstance _ si) state vl =
    let txout = Typed.makeTypedScriptTxOut si state vl
        tx = Typed.addTypedTxOut txout Typed.baseTx

    in (tx, state)

-- | Advance a running state machine contract.
--
--   This applies the transition function
--   to the current contract state. If the result is a final state, then there
--   are no further script outputs from the transaction. If it is not a final state,
--   we use the new state to lock the ongoing funds.
mkStep
    :: forall s i e m
    . (MonadError e m, AsSMError e s i, PlutusTx.IsData s, PlutusTx.IsData i)
    => SM.StateMachineInstance s i
    -- ^ The parameters of the contract instance
    -> OnChainState s i
    -- ^ Current state of the instance
    -> i
    -- ^ Input to be applied to the contract
    -> (Value -> Value)
    -- ^ Function determining how much of the existing value to pass on to the outgoing script output, if there is one.
    -> m (Either (HaltingTx s i) (SteppingTx s i), s)
    -- ^ The advancing transaction, which consumes all the outputs at the script address, and the new state after applying the input
mkStep (SM.StateMachineInstance (SM.StateMachine step _ final) si) (currentOut, currentOutRef, currentState) input valueAllocator = do
    let value = Ledger.txOutValue $ Typed.unTypedScriptTxOut currentOut

    newState <- case step currentState input of
        Just s  -> pure s
        Nothing -> throwing _SMError $ InvalidTransition currentState input

    pure $ if final newState
    then
        let txin = Typed.makeTypedScriptTxIn si input currentOutRef
            tx = Typed.addTypedTxIn txin Typed.baseTx
        in (Left tx, newState)
    else
        let txin = Typed.makeTypedScriptTxIn si input currentOutRef
            txout = Typed.makeTypedScriptTxOut si newState (valueAllocator value)
            tx = Typed.addTypedTxIn txin $ Typed.addTypedTxOut txout Typed.baseTx
        in (Right tx, newState)
