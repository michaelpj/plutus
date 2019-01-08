-- | Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction.
--
-- Note [Transactions in the crowdfunding campaign] explains the structure of
-- this contract on the blockchain.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fplugin=Language.PlutusTx.Plugin -fplugin-opt Language.PlutusTx.Plugin:dont-typecheck #-}
module Language.PlutusTx.Coordination.Contracts.CrowdFunding2 where

import qualified Language.PlutusTx            as PlutusTx
import qualified Language.PlutusTx.Prelude    as P
import           Ledger
import           Ledger.Validation
import Prelude hiding ((&&))

-- | A crowdfunding campaign.
data Campaign = Campaign
    { campaignDeadline           :: Height
    -- ^ The date by which the campaign target has to be met. (Blocks have a
    --   fixed length, so we can use them to measure time)
    , campaignTarget             :: Value
    -- ^ Target amount of funds
    , campaignCollectionDeadline :: Height
    -- ^ The date by which the campaign owner has to collect the funds
    , campaignOwner              :: PubKey
    -- ^ Public key of the campaign owner. This key is entitled to retrieve the
    --   funds if the campaign is successful.
    }

PlutusTx.makeLift ''Campaign

-- | Action that can be taken by the participants in this contract. A value of
--   `CampaignAction` is provided as the redeemer. The validator script then
--   checks if the conditions for performing this action are met.
--
data CampaignAction = Collect | Refund

PlutusTx.makeLift ''CampaignAction

-- | The validator script that determines whether the campaign owner can
--   retrieve the funds or the contributors can claim a refund.
--
contributionScript :: PlutusTx.CompiledCode (Campaign -> CampaignAction -> PubKey -> PendingTx' -> ())
contributionScript = $$(PlutusTx.compile [||

        -- The validator script is a function of four arguments:
        -- 1. The 'Campaign' definition. This argument is provided by the Plutus client, using 'Ledger.applyScript'.
        --    As a result, the 'Campaign' definition is part of the script address, and different campaigns have different addresses.
        --    The Campaign{..} syntax means that all fields of the 'Campaign' value are in scope (for example 'campaignDeadline' in l. 70).
        --    See note [RecordWildCards].
        --
        -- 2. A 'CampaignAction'. This is the redeemer script. It is provided by the redeeming transaction.
        --
        -- 3. A 'PubKey'. This is the data script. It is provided by the producing transaction (the contribution)
        --
        -- 4. A 'PendingTx' value. It contains information about the current transaction and is provided by the slot leader.
        --    See note [PendingTx]
        \Campaign{..} (act :: CampaignAction) (con :: PubKey) (p :: PendingTx') ->
            let
                -- In Haskell we can define new operators. We import
                -- `PlutusTx.and` from the Prelude here so that we can use it
                -- in infix position rather than prefix (which would require a
                -- lot of additional brackets)
                infixr 3 &&
                (&&) :: Bool -> Bool -> Bool
                (&&) = $$(PlutusTx.and)

                -- We pattern match on the pending transaction `p` to get the
                -- information we need:
                -- `ps` is the list of inputs of the transaction
                -- `outs` is the list of outputs
                -- `h` is the current block height
                PendingTx ps outs _ _ (Height h) _ _ = p

                -- `deadline` is the campaign deadline, but we need it as an
                -- `Int` so that we can compare it with other integers.
                deadline :: Int
                deadline = let Height h' = campaignDeadline in h'


                -- `collectionDeadline` is the campaign collection deadline as
                -- an `Int`
                collectionDeadline :: Int
                collectionDeadline = let Height h' = campaignCollectionDeadline in h'

                -- `target` is the campaign target as
                -- an `Int`
                target :: Int
                target = let Value v = campaignTarget in v


                -- `totalInputs` is the sum of the values of all transation
                -- inputs. We ise `foldr` from the Prelude to go through the
                -- list and sum up the values.
                totalInputs :: Int
                totalInputs =
                    let v (PendingTxIn _ _ (Value vl)) = vl in
                    $$(P.foldr) (\i total -> total + v i) 0 ps

                isValid = case act of
                    Refund -> -- the "refund" branch
                        let

                            contributorTxOut :: PendingTxOut -> Bool
                            contributorTxOut o = case $$(pubKeyOutput) o of
                                Nothing -> False
                                Just pk -> $$(eqPubKey) pk con

                            -- Check that all outputs are paid to the public key
                            -- of the contributor (this key is provided as the data script `con`)
                            contributorOnly = $$(P.all) contributorTxOut outs

                            refundable = h >= collectionDeadline && contributorOnly && $$(txSignedBy) p con

                        in refundable
                    Collect -> -- the "successful campaign" branch
                        let
                            payToOwner = h >= deadline && h < collectionDeadline && totalInputs >= target && $$(txSignedBy) p campaignOwner
                        in payToOwner
            in
            if isValid then () else $$(P.error) () ||])
