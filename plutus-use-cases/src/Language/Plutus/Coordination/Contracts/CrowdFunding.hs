-- | Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction. This is, of course, limited by the maximum
-- number of inputs a transaction can have.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fplugin=Language.Plutus.CoreToPLC.Plugin #-}
module Language.Plutus.Coordination.Contracts.CrowdFunding (
    Campaign(..)
    -- * Functionality for campaign contributors
    , contribute
    , contributionScript
    , refund
    , refundTrigger
    -- * Functionality for campaign owners
    , collect
    , collectFundsTrigger
    ) where

import           Language.Plutus.Coordination.Plutus
import           Language.Plutus.CoreToPLC.Plugin     (plc)
import qualified Language.Plutus.CoreToPLC.Primitives as Prim

import           Prelude                              (Bool (..), Maybe (..), Num (..), Ord (..), succ, sum, ($))

-- | A crowdfunding campaign.
data Campaign = Campaign
    { campaignDeadline           :: !BlockHeight
    , campaignTarget             :: !Value
    , campaignCollectionDeadline :: !BlockHeight
    , campaignOwner              :: !PubKey
    }

-- | Contribute funds to the campaign (contributor)
--
contribute :: Campaign -> Value -> TxM [TxOutRef]
contribute c value = do
    assert (value > 0)
    contributorPubKey <- lookupMyPubKey
    myPayment         <- createPayment (value + standardTxFee)
    let validator = contributionScript c (Just contributorPubKey)
        o = TxOutScript
            value
            (hash validator)
            0 -- TODO: contributorPubKey ought to be lifted into PLC at coordination runtime as the data script
    submitTransaction Tx
      { txInputs  = (myPayment, myPayment) -- TODO: Change to [myPayment] when we can have a list of inputs
      , txOutputs = (o, o)
      }
    -- the transaction above really ought to be merely a transaction *template* and the transaction fee ought to be
    -- added by the Wallet API Plutus library on the basis of the size and other costs of the transaction

-- | The validator script that determines whether the campaign owner can
--   retrieve the funds or the contributors can claim a refund.
--
--   See note [Contracts and Validator Scripts] in
--       Language.Plutus.Coordination.Contracts
contributionScript ::
       Campaign
    -> Maybe PubKey
    -> PlutusTx
contributionScript _ _  = PlutusTx inner where
    inner = plc  (\() () p Campaign{..} contribPubKey ->
        let
            -- | Check that a transaction input is signed by the private key of the given
            --   public key.
            signedBy :: TxIn -> PubKey -> Bool
            signedBy = Prim.error

            infixr 3 &&
            (&&) :: Bool -> Bool -> Bool
            (&&) = Prim.error

            infixr 3 ||
            (||) :: Bool -> Bool -> Bool
            (||) = Prim.error

            -- | Check that a pending transcation is signed by the private key of the given
            --   public key.
            signedByT :: PendingTx -> PubKey -> Bool
            signedByT = Prim.error

            maybe :: b -> (a -> b) -> Maybe a -> b
            maybe = Prim.error

            PendingTx pendingTxBlockHeight _ pendingTxTransaction = p

            Tx (t1, t2) _ = pendingTxTransaction
            TxIn (TxOutRef v1 _ _) _ = t1
            TxIn (TxOutRef v2 _ _) _ = t2


            pledgedFunds = v1 + v2
            -- Check that a refund transaction only spends the amount that was
            -- pledged by the contributor identified by `contribPubKey`
            contributorOnly = maybe False (\k ->
                                signedBy t1 k && signedBy t2 k) contribPubKey
            payToOwner   = pendingTxBlockHeight > campaignDeadline &&
                           pendingTxBlockHeight <= campaignCollectionDeadline &&
                           pledgedFunds >= campaignTarget &&
                           signedByT p campaignOwner
            -- In case of a refund, we can only collect the funds that
            -- were committed by this contributor
            refundable   = pendingTxBlockHeight > campaignCollectionDeadline &&
                           contributorOnly &&
                           maybe False (signedByT p) contribPubKey
        in
        if payToOwner || refundable then () else Prim.error)

-- | Given the campaign data and the output from the contributing transaction,
--   make a trigger that fires when the transaction can be refunded.
refundTrigger :: Campaign -> Address -> EventTrigger
refundTrigger Campaign{..} t = And
    (FundsAtAddress [t]  (GEQ 1))
    (BlockHeightRange (GEQ $ succ campaignCollectionDeadline))

-- | Given the public key of the campaign owner, generate an event trigger that
-- fires when the funds can be collected.
collectFundsTrigger :: Campaign -> [Address] -> EventTrigger
collectFundsTrigger Campaign{..} ts = And
    (FundsAtAddress ts $ GEQ campaignTarget)
    (BlockHeightRange $ Interval campaignDeadline campaignCollectionDeadline)

refund :: TxOutRef -> TxM [TxOutRef]
refund ref = do
    kp <- lookupMyKeyPair
    let i = txInSign ref kp
        o = TxOutPubKey value (pubKey kp)
    submitTransaction $ Tx {
      txInputs = (i, i),
      txOutputs = (o, o)
    } where
      value = txOutRefValue ref - standardTxFee -- TODO: Fee should be inserted by wallet

-- | Collect all campaign funds (campaign owner)
--
-- NB: Simplifing assumption: the number of contributions doesn’t
--     exceed the number of inputs that we can put into a single
--     transaction.
--
collect :: (TxOutRef, TxOutRef) -> TxM [TxOutRef]
collect (o1, o2) = do
    ownerKeyPair <- lookupMyKeyPair
    let oo = TxOutPubKey value (pubKey ownerKeyPair)
    submitTransaction Tx
      { txInputs  = (txInSign o1 ownerKeyPair, txInSign o2 ownerKeyPair)
      , txOutputs = (oo, oo)
      }
    where
      value = sum [txOutRefValue outRef | outRef <- [o1, o2]] + standardTxFee
