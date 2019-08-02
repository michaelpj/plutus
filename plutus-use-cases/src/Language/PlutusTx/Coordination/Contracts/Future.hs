{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | A futures contract in Plutus. This example illustrates three concepts.
--   1. Maintaining a margin (a kind of deposit) during the duration of the contract to protect against breach of contract (see note [Futures in Plutus])
--   2. Using oracle values to obtain current pricing information (see note [Oracles] in Language.PlutusTx.Coordination.Contracts)
--   3. Using the redeemer script to model actions that the participants in the contract may take.
module Language.PlutusTx.Coordination.Contracts.Future(
    -- * Data types
    Future(..),
    FutureData(..),
    FutureRedeemer(..),
    Role(..),
    -- * Actions
    runContract,
    -- * Script
    validatorScript,
    mkValidator
    ) where

import           Control.Eff
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Reader               (MonadReader (..), asks, runReaderT)
import           Control.Monad.State.Lazy           (MonadState (..), evalStateT, gets, modify)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Monoid
import           GHC.Generics                       (Generic)

import           Ledger                             (Address, DataScript (..), PubKey, RedeemerScript (..), Slot (..),
                                                     TxIn, TxOut,  ValidatorScript (..), Value)
import qualified Ledger                             as Ledger
import           Ledger.Ada                         (Ada)
import qualified Ledger.Ada                         as Ada
import           Ledger.AddressMap                  (AddressMap)
import qualified Ledger.AddressMap                  as AM
import qualified Ledger.Interval                    as Interval
import           Ledger.Scripts                     (HashedDataScript)
import qualified Ledger.Tx                          as LTx
import           Ledger.Validation                  (OracleValue (..), PendingTx (..), PendingTxOut (..))
import qualified Ledger.Validation                  as Validation

import           Language.Plutus.Contract
import           Language.Plutus.Contract.Resumable (Resumable)
import           Language.Plutus.Contract.Tx        (UnbalancedTx)
import qualified Language.Plutus.Contract.Tx        as Tx

import qualified Language.PlutusTx                  as PlutusTx
import           Language.PlutusTx.Prelude

{- Note [Futures in Plutus]

A futures contract ("future") is an agreement to change ownership of an
asset at a certain time (the delivery time) for an agreed price (the forward
price). The time of the transfer, and the price, are fixed at the beginning of the contract.

On the mockchain we only have one type of asset (namely, Ada coin value),
so we simply exchange the difference in price between the forward price and the
actual price. This is called a "cash settlement".

The agreement involves two parties, a buyer (long position) and a seller (short
position). At the delivery time the actual price of the asset (spot price) is
quite likely different from the forward price. If the spot price is higher than
the forward price, then the seller transfers the difference to the buyer. If
the spot price is lower than the forward price, then the buyer transfers money
to the seller. In either case there is a risk that the payer does not meet their
obligation (by simply not paying). To protect against this risk, the contract
includes a kind of deposit called "margin".

Each party deposits an initial margin. If the price moves against the seller,
then the seller has to top up their margin periodically (in our case, once each
block). Likewise, if it moves against the buyer then the buyer has to top up
their margin. If either party fails to make a margin payment then the contract
will be settled early.

The current value of the underlying asset is determined by an oracle. See note
[Oracles] in Language.PlutusTx.Coordination.Contracts.

-}

-- | Run the futures contract. Note that this does not make the initial 
--   transaction (see 'writeInitialTx')
runContract
    :: ContractActions r
    => Future
    -> PubKey
    -> PubKey
    -> Contract r ()
runContract f long short =
    flip runReaderT f $ do
        fd <- initialData long short
        addr <- contractAddress
        let initMp  = AM.addAddress addr Data.Monoid.mempty
            st = FutureState { knownOutputs = initMp, futureData = fd }
        flip evalStateT st $ 
            (liftBase (endpoint "start") >>= futureContract)
            <|>
            (liftBase (endpoint @() "initialise") >> writeInitialTx)

{- Note [Initialising the futures contract]

The margin accounts of both parties (long & short) are kept in the same 
transaction output. To initialise the contract we need a transaction that
locks the initial margins of both parties with the contract script, as is done 
in 'writeInitialTx', which is called from the "initialise" endpoint of the 
contract. In the test suite we simply call "initialise" from one of the 
emulator's wallets. To make this more realistic we could use an escrow contract 
that takes the initial margins and then locks them using the script.

TODO: Implement escrow contract that pays to a script address, and use it here.

-}

data Role = Long | Short
    deriving (Generic, Show, FromJSON, ToJSON)

-- | The current state of the futures contract.
data FutureState =
    FutureState
        { knownOutputs :: AddressMap
        -- ^ The unspent outputs (usually exactly 1) of the contract instance.
        , futureData   :: FutureData
        -- ^ The current margin accounts
        }

type MonadFuture r m =
    ( MonadState FutureState m
    , MonadReader Future m
    , MonadBase (Resumable (Eff r)) m)
-- TODO: ResumableT ?

-- | Spend all known outputs at the contract address using the 'FutureRedeemer'.
--   Use 'redeemWith0' instead if the transaction does not have a pay-to-script 
--   output.
redeemWith
    :: MonadFuture r m
    => FutureRedeemer
    -> m [TxIn]
redeemWith redeemer = do
    valScript <- asks validatorScript
    let redScript = redeemerScript redeemer
    gets (AM.spendScriptOutputs valScript redScript . knownOutputs)

-- | Spend all known outputs at the contract address using the 'FutureRedeemer'.
redeemWith0
    :: MonadFuture r m
    => FutureRedeemer
    -> m [TxIn]
redeemWith0 redeemer = do
    valScript <- asks validatorScript
    let redScript = redeemerScript0 redeemer
    gets (AM.spendScriptOutputs valScript redScript . knownOutputs)

-- | Lock some Ada in the contract using the 'FutureData' as data script.
payIntoContract
    :: MonadReader Future m
    => FutureData
    -> Ada
    -> m TxOut
payIntoContract fd vl = do
    address <- contractAddress
    let datScript = DataScript $ Ledger.lifted fd
    pure $ LTx.scriptTxOut' (Ada.toValue vl) address datScript

-- | Initialise the futures contract by paying the initial margin.
writeInitialTx
    :: ( Member WriteTx r
       , MonadFuture r m )
    => m ()
writeInitialTx = do
    dt <- gets futureData
    initialMargin <- asks futureInitialMargin
    out <- payIntoContract dt (2 * initialMargin)
    liftBase $ writeTx $ Tx.unbalancedTx [] [out]

-- | The 'FutureData' for the two parties (long and short) with the
--   initial margin.
initialData
    :: ( MonadReader Future m )
    => PubKey
    -> PubKey
    -> m FutureData
initialData long short = do
    f <- ask
    let initialMargin = futureInitialMargin f
    pure (FutureData long short initialMargin initialMargin)

-- | Create a transaction that closes the futures contract by paying out
--   what remains of each party's margin.
settlement
    :: (MonadFuture r m)
    => OracleValue Ada
    -> m UnbalancedTx
settlement ov = do
    fd <- gets futureData
    ft <- ask
    let
        forwardPrice = futureUnitPrice ft
        OracleValue _ _ spotPrice = ov
        delta = (Ada.fromInt $ futureUnits ft) * (spotPrice - forwardPrice)
        longOut = Ada.toValue (futureDataMarginLong fd + delta)
        shortOut = Ada.toValue (futureDataMarginShort fd - delta)
        outs =
            [ Ledger.pubKeyTxOut longOut (futureDataLong fd)
            , Ledger.pubKeyTxOut shortOut (futureDataShort fd)
            ]
    ins <- redeemWith0 (Settle ov)
    pure $ Tx.unbalancedTx ins outs
            & validityRange .~ Interval.from (futureDeliveryDate ft)

-- | Settle the contract at the end of its life span.
settleC
    :: (MonadFuture r m
       , Member WriteTx r)
    => OracleValue Ada
    -> m ()
settleC ov =
    settlement ov >>= liftBase . writeTx

-- | Settle the position early if a margin payment has been missed.
settleEarlyC
    :: (MonadFuture r m
       , Member WriteTx r)
    => OracleValue Ada
    -> m ()
settleEarlyC ov = do
    ins <- redeemWith0 (Settle ov)
    liftBase (writeTx $ Tx.unbalancedTx ins [])

-- | Increase the margin by adding some Ada
adjustMarginC
    :: (MonadFuture r m, Member WriteTx r)
    => Role
    -> Ada
    -> m ()
adjustMarginC role vl = do
    fd <- gets futureData
    let fd' = case role of
            Long  -> fd { futureDataMarginLong  = vl + futureDataMarginLong fd }
            Short -> fd { futureDataMarginShort = vl + futureDataMarginShort fd }
        newValue = futureDataMarginLong fd' + futureDataMarginShort fd'
    out <- payIntoContract fd' newValue
    ins <- redeemWith AdjustMargin
    _ <- modify (\s -> s { futureData = fd' })
    liftBase $ writeTx $ Tx.unbalancedTx ins [out]

-- | The address of this contract instance.
contractAddress :: MonadReader Future m => m Address
contractAddress = asks (Ledger.scriptAddress . validatorScript)

totalValueLocked
    :: ( MonadReader Future m )
    => AM.AddressMap
    -> m Value
totalValueLocked am = do
    addr <- contractAddress
    pure $ AM.values am ^. at addr . _Just

data Liveness = Alive | Dead
    deriving Show

-- | Watch the on-chain state of the contract for changes and expose
--   three endpoints for adjusting the margin, settling early, and settling
--   regularly.
futureContract
    :: ( MonadFuture r m
       , ContractActions r
       , Alternative m)
    => Role
    -> m ()
futureContract r =
    let adjustMarginEP = liftBase (endpoint "adjust margin") >>= adjustMarginC r
        settleEarlyEP  = liftBase (endpoint "settle early") >>= settleEarlyC
        settleEP       = liftBase (endpoint "settle") >>= settleC
        go = do
                alive <- selectEither updateState (adjustMarginEP <|> settleEarlyEP <|> settleEP)
                case alive of
                    Left Dead -> pure ()
                    _         -> go
    in go

-- | Wait for a change to the contract state on-chain, update the
--   state, then return 'Liveness' value indicating whether the contract
--   is still going.
updateState
    :: ( MonadFuture r m, Member WatchAddress r )
    => m Liveness
updateState = do
    addr <- contractAddress
    tx <- liftBase (nextTransactionAt addr)
    -- We have a transaction 'tx' that modifies the UTXO set at the contract
    -- address.
    -- We now need to update the 'futureData' field of the state with the new
    -- 'FutureData'. We could get the new 'FutureData' by unlifting the data
    -- script from script output of 'tx', BUT we can't unlift things yet. But
    -- we can infer what the new state is from the following facts.
    --
    -- 1. The only two state-changing actions are 'AdjustMargin' and 'Settle'.
    -- 2. If the action performed was 'Settle' then 'tx' does not have any
    --    outputs at the script address because the instance is done.
    -- 3. If the action was 'AdjustMargin', then either the
    --    'futureDataMarginShort' or the 'futureDataMarginLong' field has
    --    changed. The delta is exactly the delta of Ada that was locked by the
    --    contract previously vs the amount that is locked by 'tx'.

    oldState <- gets futureData
    lockedPrior <- gets knownOutputs >>= totalValueLocked
    lockedNow   <- totalValueLocked (AM.fromTxOutputs tx)
    let delta = Ada.fromValue lockedNow - Ada.fromValue lockedPrior
        newDS = listToMaybe $ mapMaybe LTx.txOutData $ view LTx.outputs tx
        fd1   = oldState { futureDataMarginLong = futureDataMarginLong oldState + delta }
        fd2   = oldState { futureDataMarginShort = futureDataMarginShort oldState + delta }
    case newDS of
        Just ds'
            | ds' == DataScript (Ledger.lifted fd1) -> do
                modify $ \s -> s { futureData = fd1, knownOutputs = AM.updateAddresses tx (knownOutputs s) }
                pure Alive
            | ds' == DataScript (Ledger.lifted fd2) -> do
                modify $ \s -> s { futureData = fd2, knownOutputs = AM.updateAddresses tx (knownOutputs s) }
                pure Alive
            | ds' == DataScript (Ledger.lifted oldState) -> do
                -- Nothing changed (this happens when the contract is first
                -- initialised)
                modify $ \s -> s { knownOutputs = AM.fromTxOutputs tx }
                pure Alive
        _ -> pure Dead

-- | Basic data of a futures contract. `Future` contains all values that do not
--   change during the lifetime of the contract.
--
data Future = Future {
    futureDeliveryDate  :: Slot,
    futureUnits         :: Integer,
    futureUnitPrice     :: Ada,
    futureInitialMargin :: Ada,
    futurePriceOracle   :: PubKey,
    futureMarginPenalty :: Ada
    -- ^ How much a participant loses if they fail to make the required margin
    --   payments.
    } deriving Generic

-- | The current "state" of the futures contract. `FutureData` contains values
--   that may change during the lifetime of the contract. This is the data
--   script.
--
data FutureData = FutureData {
    futureDataLong        :: PubKey,
    -- ^ Holder of the long position (buyer)
    futureDataShort       :: PubKey,
    -- ^ Holder of the short position (seller)
    futureDataMarginLong  :: Ada,
    -- ^ Current balance of the margin account of the long position
    futureDataMarginShort :: Ada
    -- ^ Current balance of the margin account of the short position
    } deriving Generic

-- | Actions that either participant may take. This is the redeemer script.
data FutureRedeemer =
      AdjustMargin
    -- ^ Make a margin payment
    | Settle (OracleValue Ada)
    -- ^ Settle the contract
    deriving Generic

-- | Compute the required margin from the current price of the
--   underlying asset.
requiredMargin :: Future -> Ada -> Ada
requiredMargin Future{futureUnits=units, futureUnitPrice=unitPrice, futureMarginPenalty=pnlty} spotPrice =
    let
        delta  = Ada.multiply (Ada.fromInt units) (Ada.minus spotPrice unitPrice)
    in
        Ada.plus pnlty delta

redeemerScript :: FutureRedeemer -> RedeemerScript
redeemerScript fr = RedeemerScript $
    $$(Ledger.compileScript [|| \(d :: FutureRedeemer) -> \(_ :: Sealed (HashedDataScript FutureData)) -> d ||])
        `Ledger.applyScript`
            Ledger.lifted fr

redeemerScript0 :: FutureRedeemer -> RedeemerScript
redeemerScript0 fr = RedeemerScript $ Ledger.lifted fr

{-# INLINABLE mkValidator #-}
mkValidator :: Future -> FutureData -> FutureRedeemer -> PendingTx -> Bool
mkValidator ft@Future{..} FutureData{..} r p@PendingTx{pendingTxOutputs=outs, pendingTxValidRange=range} =
    let

        isPubKeyOutput :: PendingTxOut -> PubKey -> Bool
        isPubKeyOutput o k = maybe False ((==) k) (Validation.pubKeyOutput o)

        --  | Check if a `PendingTxOut` is a public key output for the given pub. key and ada value
        paidOutTo :: Ada -> PubKey -> PendingTxOut -> Bool
        paidOutTo vl pk txo =
            let PendingTxOut vl' _ _ = txo
                adaVl' = Ada.fromValue vl'
            in
            isPubKeyOutput txo pk && vl == adaVl'

        verifyOracle :: OracleValue a -> (Slot, a)
        verifyOracle (OracleValue pk h t) =
            if pk == futurePriceOracle then (h, t) else error ()

    in case r of
            -- Settling the contract is allowed if any of three conditions hold:
            --
            -- 1. The `deliveryDate` has been reached. In this case both parties get what is left of their margin
            -- plus/minus the difference between spot and forward price.
            -- 2. The owner of the long position has failed to make a margin payment. In this case the owner of the short position gets both margins.
            -- 3. The owner of the short position has failed to make a margin payment. In this case the owner of the long position gets both margins.
            --
            -- In case (1) there are two payments (1 to each of the participants). In cases (2) and (3) there is only one payment.

            Settle ov ->
                let
                    spotPrice = snd (verifyOracle ov)
                    delta  = Ada.multiply (Ada.fromInt futureUnits) (Ada.minus spotPrice futureUnitPrice)
                    expShort = Ada.minus futureDataMarginShort delta
                    expLong  = Ada.plus futureDataMarginLong delta
                    slotvalid = Interval.member futureDeliveryDate range

                    canSettle =
                        case outs of
                            o1:o2:_ ->
                                let paymentsValid =
                                        (paidOutTo expShort futureDataShort o1 && paidOutTo expLong futureDataLong o2)
                                        || (paidOutTo expShort futureDataShort o2 && paidOutTo expLong futureDataLong o1)
                                in
                                    slotvalid && paymentsValid
                            o1:_ ->
                                let
                                    totalMargin = Ada.plus futureDataMarginShort futureDataMarginLong
                                    reqMargin   = requiredMargin ft spotPrice
                                    case2 = futureDataMarginLong < reqMargin
                                            && paidOutTo totalMargin futureDataShort o1

                                    case3 = futureDataMarginShort < reqMargin
                                            && paidOutTo totalMargin futureDataLong o1

                                in
                                    case2 || case3
                            _ -> False

                in
                    canSettle

            -- For adjusting the margin we simply check that the amount locked in the contract
            -- is larger than it was before.
            --
            AdjustMargin ->
                let
                    ownHash = fst (Validation.ownHashes p)
                    vl = Validation.adaLockedBy p ownHash
                in
                    vl > (futureDataMarginShort `Ada.plus` futureDataMarginLong)

validatorScript :: Future -> ValidatorScript
validatorScript ft = ValidatorScript $
    $$(Ledger.compileScript [|| mkValidator ||])
        `Ledger.applyScript`
            Ledger.lifted ft

PlutusTx.makeLift ''Future
PlutusTx.makeLift ''FutureData
PlutusTx.makeLift ''FutureRedeemer
