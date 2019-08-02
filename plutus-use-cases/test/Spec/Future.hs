{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Spec.Future(tests) where

import           Test.Tasty
import qualified Test.Tasty.HUnit                                as HUnit

import qualified Spec.Lib                                        as Lib

import qualified Ledger
import           Ledger.Ada                                      (Ada)
import qualified Ledger.Ada                                      as Ada
import           Ledger.Crypto                                   (PubKey (..))
import           Ledger.Validation                               (OracleValue (..))
import qualified Ledger.Value                                    as Value

import           Language.Plutus.Contract.Test
import qualified Language.Plutus.Contract.Trace                  as Trace
import qualified Language.PlutusTx                               as PlutusTx
import           Language.PlutusTx.Coordination.Contracts.Future (Future (..))
import qualified Language.PlutusTx.Coordination.Contracts.Future as F

-- | Wallet 1. Holder of the "long" position in the contract.
wallet1 :: Wallet
wallet1 = Wallet 1

-- | Wallet 2. Holder of the "short" position in the contract.
wallet2 :: Wallet
wallet2 = Wallet 2

-- | Wallet 3. Initialises the contract.
wallet3 :: Wallet
wallet3 = Wallet 3

tests :: TestTree
tests =
    let con = F.runContract contract (walletPubKey wallet1) (walletPubKey wallet2) in
    testGroup "futures"
    [ checkPredicate "initialise future"
        con
        (walletFundsChange wallet3 (Ada.toValue $ negate (2 * initMargin)))
        $ Trace.callEndpoint wallet3 "initialise" ()
            >> Trace.handleBlockchainEvents wallet3

    , checkPredicate "close the position"
        con
        (walletFundsChange wallet1 (Ada.toValue $ initMargin + delta 1124)
            <> walletFundsChange wallet2 (Ada.toValue $ initMargin - delta 1124)
            <> walletFundsChange wallet3 (Ada.toValue $ negate (2 * initMargin)))
        (settleTrace 1124 8 "settle")

    , checkPredicate "close early if margin payment was missed"
        con
        (walletFundsChange wallet1 (Ada.toValue $ 2 * initMargin)
            <> walletFundsChange wallet2 Value.zero
            <> walletFundsChange wallet3 (Ada.toValue $ negate (2 * initMargin)))
        (let (_, upper) = marginRange
             spotPrice = upper + 1
        in settleTrace spotPrice 2 "settle early")

    , checkPredicate "increase the margin"
        con
        (walletFundsChange wallet1 (Ada.adaValueOf (-100)))
        (initialiseTrace
            >> Trace.callEndpoint wallet1 "adjust margin" (100 :: Ada)
            >> Trace.handleBlockchainEvents wallet1)

    , Lib.goldenPir "test/Spec/future.pir" $$(PlutusTx.compile [|| F.mkValidator ||])
    , HUnit.testCase "script size is reasonable" (Lib.reasonable (F.validatorScript contract) 50000)
    ]

-- | Start the contract with wallet 1 as the short position, wallet 2 as the long
--   position, and wallet 3 as the wallet that makes the initial transaction (see note
--   "Initialising the futures contract".)
initialiseTrace
    :: ( MonadEmulator m )
    => ContractTrace m a ()
initialiseTrace = do
    Trace.callEndpoint wallet1 "start" F.Long
    Trace.notifyInterestingAddresses wallet1
    Trace.callEndpoint wallet2 "start" F.Short
    Trace.notifyInterestingAddresses wallet2
    Trace.callEndpoint wallet3 "initialise" ()
    Trace.handleBlockchainEvents wallet3

-- | Settle the contract with the given spot price, after adding some
--   blocks and using the endpoint provided.
settleTrace
    :: ( MonadEmulator m )
    => Ada
    -- ^ Spot price
    -> Integer
    -- ^ How many blocks to add
    -> String
    -- ^ Name of endpoint
    -> ContractTrace m a ()
settleTrace spotPrice blocks ep = do
    initialiseTrace
    Trace.addBlocks blocks
    let ov = OracleValue oracle (Ledger.Slot (blocks + 2)) spotPrice
    Trace.callEndpoint wallet1 ep ov
    Trace.handleBlockchainEvents wallet1

delta :: Ada -> Ada
delta spotPrice = fromIntegral units * (spotPrice - forwardPrice)

-- | A futures contract over 187 units with a forward price of 1233, due at
--   10 blocks.
contract :: Future
contract = Future {
    futureDeliveryDate  = Ledger.Slot 10,
    futureUnits         = units,
    futureUnitPrice     = forwardPrice,
    futureInitialMargin = im,
    futurePriceOracle   = oracle,
    futureMarginPenalty = penalty
    } where
        im = penalty + (Ada.fromInt units * forwardPrice `div` 20) -- 5%

-- | Margin penalty
penalty :: Ada
penalty = 1000

-- | The forward price agreed at the beginning of the contract.
forwardPrice :: Ada
forwardPrice = 1123

-- | Range within which the underlying asset's price can move before the first
--   margin payment is necessary.
--   If the price approaches the lower range, then the buyer (long position,
--   Wallet 1) has to increase their margin, and vice versa.
marginRange :: (Ada, Ada)
marginRange = (forwardPrice - limit, forwardPrice + limit) where
    limit = forwardPrice `div` 20

-- | How many units of the underlying asset are covered by the contract.
units :: Integer
units = 187

oracle :: PubKey
oracle = walletPubKey (Wallet 4)

initMargin :: Ada
initMargin = futureInitialMargin contract

