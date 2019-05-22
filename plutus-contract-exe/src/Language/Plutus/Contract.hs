{-# LANGUAGE ScopedTypeVariables #-}
module Language.Plutus.Contract(
    PlutusContract
    , watchAddress
    , endpoint
    , writeTx
    , fundsAtAddressGt
    , emit
    ) where

import           Control.Lens
import           Control.Monad                        ((>=>))
import           Data.Aeson                           (FromJSON)
import qualified Data.Aeson                           as Aeson
import           Data.Maybe                           (fromMaybe)

import           Language.Plutus.Contract.Contract    as Contract
import           Language.Plutus.Contract.Event       as Event
import           Language.Plutus.Contract.Transaction as Transaction

import           Ledger.AddressMap                    (AddressMap)
import qualified Ledger.AddressMap                    as AM
import           Ledger.Tx                            (Address, Tx)
import           Ledger.Value                         (Value)
import qualified Ledger.Value                         as V

type PlutusContract a = Contract Step Event a

-- | Watch an 'Address', returning the next transaction that changes it
watchAddress :: Address -> PlutusContract Tx
watchAddress a = await (Event.addr a) (ledgerUpdate >=> check) where
    check (a', t)
        | a == a' = Just t
        | otherwise = Nothing

-- | Expose an endpoint, returning the data that was entered
endpoint :: forall a. FromJSON a => String -> PlutusContract a
endpoint nm = await (Event.endpointName nm) (endpointEvent >=> uncurry dec) where
    dec :: String -> Aeson.Value -> Maybe a
    dec nm' vl
        | nm' == nm =
            case Aeson.fromJSON vl of
                Aeson.Success r -> Just r
                _               -> Nothing
        | otherwise = Nothing

-- | Produce an unbalanced transaction
writeTx :: UnbalancedTx -> PlutusContract ()
writeTx = emit . Event.tx

-- | Watch an address for changes, and return the outputs
--   at that address when the total value at the address
--   has surpassed the given value.
fundsAtAddressGt :: Address -> Value -> PlutusContract AddressMap
fundsAtAddressGt addr' vl = loopM go mempty where
    go cur = do
        delta <- AM.fromTxOutputs <$> watchAddress addr'
        let cur' = cur <> delta
            presentVal = fromMaybe mempty (AM.values cur' ^. at addr')
        if presentVal `V.gt` vl
        then pure (Left cur') else pure (Right cur')
