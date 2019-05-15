{-# LANGUAGE ScopedTypeVariables #-}
module Language.Plutus.Contract(
    PlutusContract
    , watchAddress
    , endpoint
    , emit
    ) where

import           Control.Monad                        ((>=>))
import           Data.Aeson                           (FromJSON)
import qualified Data.Aeson                           as Aeson

import           Language.Plutus.Contract.Contract    as Contract
import           Language.Plutus.Contract.Event       as Event
import           Language.Plutus.Contract.Transaction as Transaction

import           Ledger.Tx                            (Address, Tx)

type PlutusContract a = Contract Step Event a

-- | Watch an 'Address', returning the next transaction that changes it
watchAddress :: Address -> PlutusContract Tx
watchAddress a = await (Event.addr a) (fmap snd <$> ledgerUpdate) -- TODO: Check that tx actually involves the address

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
emit :: UnbalancedTx -> PlutusContract ()
emit t = Emit (Event.tx t) $ Pure ()
