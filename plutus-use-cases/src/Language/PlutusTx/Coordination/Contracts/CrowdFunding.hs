-- | Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction. This is, of course, limited by the maximum
-- number of inputs a transaction can have.
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
module Language.PlutusTx.Coordination.Contracts.CrowdFunding (
    -- * Campaign parameters
    Campaign(..)
    , crowdfunding
    , theCampaign
    -- * Functionality for campaign contributors
    , contribute
    -- * Functionality for campaign owners
    , scheduleCollection
    , campaignAddress
    -- * Validator script
    , contributionScript
    , mkValidator
    , mkCampaign
    , CampaignAction(..)
    , collectionRange
    , refundRange
    -- * Traces
    , startCampaign
    , makeContribution
    , successfulCampaign
    ) where

import           Control.Lens                   ((&), (.~), (^.))
import           Control.Monad                  (void)
import qualified Data.Set                       as Set
import           Language.Plutus.Contract
import           Language.Plutus.Contract.Trace (ContractTrace, MonadEmulator)
import qualified Language.Plutus.Contract.Trace as Trace
import qualified Language.PlutusTx              as PlutusTx
import           Language.PlutusTx.Prelude hiding ((>>), return, (>>=))
import           Ledger                         (Address, PendingTx, PubKey, Slot, ValidatorScript)
import qualified Ledger                         as Ledger
import qualified Ledger.Ada                     as Ada
import qualified Ledger.Interval                as Interval
import           Ledger.Slot                    (SlotRange)
import           Ledger.Validation              as V
import           Ledger.Value                   (Value)
import qualified Ledger.Value                   as VTH
import           Wallet.Emulator                (Wallet)
import qualified Wallet.Emulator                as Emulator
import Language.PlutusTx.Coordination.Contracts.Crowdfunding.Types


import qualified Prelude as P

-- | Construct a 'Campaign' value from the campaign parameters,
--   using the wallet's public key.
mkCampaign :: Slot -> Value -> Slot -> Wallet -> Campaign
mkCampaign ddl target collectionDdl ownerWallet =
    Campaign
        { campaignDeadline = ddl
        , campaignTarget   = target
        , campaignCollectionDeadline = collectionDdl
        , campaignOwner = Emulator.walletPubKey ownerWallet
        }

-- | The 'SlotRange' during which the funds can be collected
collectionRange :: Campaign -> SlotRange
collectionRange cmp =
    Interval.interval (campaignDeadline cmp) (campaignCollectionDeadline cmp)

-- | The 'SlotRange' during which a refund may be claimed
refundRange :: Campaign -> SlotRange
refundRange cmp =
    Interval.from (campaignCollectionDeadline cmp)


type CrowdfundingValidator = PubKey -> CampaignAction -> PendingTx -> Bool

validRefund :: Campaign -> PubKey -> PendingTx -> Bool
validRefund campaign contributor ptx =
    Interval.contains (refundRange campaign) (pendingTxValidRange ptx)
    && (ptx `V.txSignedBy` contributor)

validCollection :: Campaign -> PendingTx -> Bool
validCollection campaign p =
    (collectionRange campaign `Interval.contains` pendingTxValidRange p)
    && (valueSpent p `VTH.geq` campaignTarget campaign)
    && (p `V.txSignedBy` campaignOwner campaign)

mkValidator :: Campaign -> CrowdfundingValidator
mkValidator c con act p = case act of
    Refund  -> validRefund c con p
    Collect -> validCollection c p

-- | The validator script that determines whether the campaign owner can
--   retrieve the funds or the contributors can claim a refund.
--
contributionScript :: Campaign -> ValidatorScript
contributionScript cmp  = Ledger.ValidatorScript $
    $$(Ledger.compileScript [|| mkValidator ||])
        `Ledger.applyScript`
            Ledger.lifted cmp

-- | The address of a [[Campaign]]
campaignAddress :: Campaign -> Ledger.Address
campaignAddress = Ledger.scriptAddress . contributionScript

-- | The crowdfunding contract for the 'Campaign'.
crowdfunding :: Campaign -> Contract _ _ ()
crowdfunding c = contribute c <|> scheduleCollection c

-- | A sample campaign with a target of 20 Ada by slot 20
theCampaign :: Campaign
theCampaign = Campaign
    { campaignDeadline = 20
    , campaignTarget   = Ada.adaValueOf 20
    , campaignCollectionDeadline = 30
    , campaignOwner = Emulator.walletPubKey (Emulator.Wallet 1)
    }

-- | The "contribute" branch of the contract for a specific 'Campaign'. Exposes 
--   an endpoint that allows the user to enter their public key and the 
--   contribution. Then waits until the campaign is over, and collects the 
--   refund if the funding target was not met.
contribute :: Campaign -> Contract _ _ ()
contribute cmp = do
    (ownPK, contribution) <- endpoint @"contribute" @(PubKey, Value)
    let ds = Ledger.DataScript (Ledger.lifted ownPK)
        tx = payToScript contribution (campaignAddress cmp) ds
                & validityRange .~ Ledger.interval 1 (campaignDeadline cmp)
    writeTx tx
    
    utxo <- watchAddressUntil (campaignAddress cmp) (campaignCollectionDeadline cmp)
    -- 'utxo' is the set of unspent outputs at the campaign address at the
    -- collection deadline. If 'utxo' still contains our own contribution
    -- then we can claim a refund.
    
    -- Finding "our" output is a bit fiddly since we don't know the transaction 
    -- ID of 'tx'. So we use `collectFromScriptFilter` to collect only those 
    -- outputs whose data script is our own public key (in 'ds')
    let flt _ txOut = Ledger.txOutData txOut == Just ds
        tx' = collectFromScriptFilter flt utxo (contributionScript cmp) (Ledger.RedeemerScript (Ledger.lifted Refund))
                & validityRange .~ refundRange cmp
    if not . Set.null $ tx' ^. inputs
    then void (writeTx tx')
    else pure ()

-- | The campaign owner's branch of the contract for a given 'Campaign'. It 
--   watches the campaign address for contributions and collects them if
--   the funding goal was reached in time.
scheduleCollection :: Campaign -> Contract _ _ ()
scheduleCollection cmp = do

    -- Expose an endpoint that lets the user fire the starting gun on the 
    -- campaign. (This endpoint isn't technically necessary, we could just
    -- run the 'trg' action right away)
    () <- endpoint @"schedule collection" @()

    -- 'trg' describes the conditions for a successful campaign. It returns a
    -- tuple with the unspent outputs at the campaign address, and the current
    -- slot.
    let trg = both
                (fundsAtAddressGt (campaignAddress cmp) (campaignTarget cmp))
                (awaitSlot (campaignDeadline cmp))

    -- We can only collect the contributions if 'trg' returns before the
    -- campaign collection deadline, so we use the 'timeout' combinator.
    void $ timeout (campaignCollectionDeadline cmp) $ do
        (outxo, _) <- trg
        let
            redeemerScript = Ledger.RedeemerScript (Ledger.lifted Collect)
            tx = collectFromScript outxo (contributionScript cmp) redeemerScript
                    & validityRange .~ collectionRange cmp
        writeTx tx

-- | Call the "schedule collection" endpoint and instruct the campaign owner's 
--   wallet (wallet 1) to start watching the campaign address.
startCampaign
    :: ( MonadEmulator m 
       , HasEndpoint "schedule collection" () ρ σ
       , AddressPrompt ρ σ
       , ContractTypes ρ σ 
       )
    => ContractTrace ρ σ m a ()
startCampaign =
    Trace.callEndpoint @"schedule collection" (Trace.Wallet 1)  ()
    P.>> Trace.notifyInterestingAddresses (Trace.Wallet 1)

-- | Call the "contribute" endpoint, contributing the amount from the wallet
makeContribution
    :: ( MonadEmulator m 
       , HasEndpoint "contribute" (PubKey, Value) ρ σ
       , AddressPrompt ρ σ
       , TxPrompt ρ σ
       , ContractTypes ρ σ 
       )
    => Wallet
    -> Value
    -> ContractTrace ρ σ m a ()
makeContribution w v =
    Trace.callEndpoint @"contribute" w (Trace.walletPubKey w, v)
        P.>> Trace.handleBlockchainEvents w

-- | Run a successful campaign with contributions from wallets 2, 3 and 4.
successfulCampaign
    :: ( MonadEmulator m
       , HasEndpoint "contribute" (PubKey, Value) ρ σ
       , HasEndpoint "schedule collection" () ρ σ
       , AddressPrompt ρ σ
       , TxPrompt ρ σ
       , SlotPrompt ρ σ
       , ContractTypes ρ σ 
       )
    => ContractTrace ρ σ m a ()
successfulCampaign =
    startCampaign
        P.>> makeContribution (Trace.Wallet 2) (Ada.adaValueOf 10)
        P.>> makeContribution (Trace.Wallet 3) (Ada.adaValueOf 10)
        P.>> makeContribution (Trace.Wallet 4) (Ada.adaValueOf 1)
        P.>> Trace.addBlocks 18
        P.>> Trace.notifySlot (Trace.Wallet 1)
        P.>> Trace.handleBlockchainEvents (Trace.Wallet 1)
