{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Language.Plutus.Contract.StateMachine where

import           Control.Applicative
import           Control.Monad.State hiding (state)
import           Control.Monad.Reader
import           Control.Monad.Base
import           Control.Monad.Error.Lens
import           Control.Lens

import qualified Data.Text as T

import           Language.Plutus.Contract hiding (when)
import qualified Language.Plutus.Contract.Tx as Contract

import qualified Language.PlutusTx as PlutusTx
import qualified Language.PlutusTx.StateMachine as SM
import           Language.PlutusTx.StateMachine ()

import qualified Ledger.Typed.Tx as Typed
import Ledger hiding (initialise)

import qualified Wallet.Typed.StateMachine as SM

import Debug.Trace

type SMSchema s i =
    BlockchainActions
        .\/ Endpoint "initialise" (s, Value)
        .\/ Endpoint "step" i

data SMContractError s i = SMError (SM.SMError s i) | ChooserError T.Text
makeClassyPrisms ''SMContractError

instance (Show s, Show i) => Show (SMContractError s i) where
    show (SMError e) = show e
    show (ChooserError t) = show t

instance SM.AsSMError (SMContractError s i) s i where
    _SMError = _SMError

data StateMachineClient s i = StateMachineClient
    { scInstance :: SM.StateMachineInstance s i
    , scPayments :: s -> i -> UnbalancedTx
    , scChooser :: [SM.OnChainState s i] -> Either (SMContractError s i) (SM.OnChainState s i)
    }

type MonadSM s i e m =
    ( MonadBase (Contract (SMSchema s i) e) m
    , AsSMContractError e s i, SM.AsSMError e s i
    , PlutusTx.IsData s, PlutusTx.IsData i
    , MonadReader (StateMachineClient s i) m
    )

-- | The address of this machine instance.
contractAddress :: MonadReader (StateMachineClient s i) m => m Address
contractAddress = asks (SM.machineAddress . scInstance)

data Liveness = NotStarted | Running | Finished

run :: forall s i e m . (MonadSM s i e m, Alternative m) => m ()
run = go NotStarted
    where
        go :: Liveness -> m ()
        go live = do
            status <- case live of
                NotStarted -> initialise
                Running -> step
                Finished -> pure Finished
            case status of
                Finished -> pure ()
                newLive -> go newLive

getOnChainState :: forall s i e m . (MonadSM s i e m) => m (SM.OnChainState s i)
getOnChainState = do
    si <- asks scInstance
    chooser <- asks scChooser
    utxo <- liftBase $ utxoAt $ SM.machineAddress si
    let states = SM.getStates si utxo
    liftBase $ either (throwing _SMContractError) pure $ chooser states

initialise :: forall s i e m . (MonadSM s i e m) => m Liveness
initialise = do
    sc <- ask

    (initialState, vl) <- liftBase $ endpoint @"initialise" @(s, Value)

    let (tx, _) = SM.mkInitialise (scInstance sc) initialState vl

    void $ liftBase $ writeTx $ Contract.fromLedgerTx $ Typed.toUntypedTx tx
    pure Running

step :: forall s i e m . (MonadSM s i e m) => m Liveness
step = do
    sc <- ask

    input <- liftBase $ endpoint @"step" @i
    currentState@(_, _, s) <- getOnChainState

    (tx, _) <- liftBase $ SM.mkStep (scInstance sc) currentState input id
    let payments = scPayments sc s input

    traceShowM payments

    case tx of
        Left halting -> do
            void $ liftBase $ writeTx $ payments <> (Contract.fromLedgerTx $ Typed.toUntypedTx halting)
            pure Finished
        Right stepping -> do
            void $ liftBase $ writeTx $ payments <> (Contract.fromLedgerTx $ Typed.toUntypedTx stepping)
            pure Running
