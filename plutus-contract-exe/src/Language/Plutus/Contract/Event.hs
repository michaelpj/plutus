{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
module Language.Plutus.Contract.Event(
      Step(..)
    , tx
    , addr
    , slot
    , endpointName
    , Event(..)
    , ledgerUpdate
    , slotChange
    , endpointEvent
    ) where

import qualified Data.Aeson                           as Aeson
import           Data.Semigroup                       (Min (..), Option (..))
import qualified Data.Set                             as Set
import           GHC.Generics                         (Generic)

import           Language.Plutus.Contract.Transaction (UnbalancedTx)
import           Ledger.Slot                          (Slot)
import           Ledger.Tx                            (Address, Tx)

data Step =
    Step
        { stepTransactions :: [UnbalancedTx]
        , stepAddresses    :: Set.Set Address
        , stepNextSlot     :: Maybe Slot
        , stepEndpoints    :: Set.Set String -- TODO: better type - Map String EndpointSchema?
        }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup Step where
    l <> r = Step
                { stepTransactions = stepTransactions l <> stepTransactions r
                , stepAddresses    = stepAddresses l <> stepAddresses r
                , stepNextSlot     = fmap getMin $ getOption $ Option (Min <$> stepNextSlot l) <> Option (Min <$> stepNextSlot r)
                , stepEndpoints    = stepEndpoints l <> stepEndpoints r
                }

instance Monoid Step where
    mappend = (<>)
    mempty = Step mempty mempty Nothing mempty

tx :: UnbalancedTx -> Step
tx t = mempty { stepTransactions = [t] }

addr :: Address -> Step
addr a = mempty { stepAddresses = Set.singleton a }

slot :: Slot -> Step
slot s = mempty { stepNextSlot = Just s }

endpointName :: String -> Step
endpointName e = mempty { stepEndpoints = Set.singleton e }

data Event =
    LedgerUpdate Address Tx
    | SlotChange Slot
    | Endpoint String Aeson.Value
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON) -- TODO: Make sure this can be created easily by app platform

ledgerUpdate :: Event -> Maybe (Address, Tx)
ledgerUpdate = \case
    LedgerUpdate a t -> Just (a, t)
    _ -> Nothing

slotChange :: Event -> Maybe Slot
slotChange = \case
    SlotChange sl -> Just sl
    _ -> Nothing

endpointEvent :: Event -> Maybe (String, Aeson.Value)
endpointEvent = \case
    Endpoint s v -> Just (s, v)
    _ -> Nothing

{-
    Event-based interface between contract executables and the app platform and
    (by extension) the wallet.

    Two types of events are defined:

    1. 'ContractOut'. Events produced by the contract for consumption by app
       platform and wallet. Includes transactions and instructions to start
       watching interesting addresses on the ledger.

    2. 'LedgerUpdate'. Events that inform the contract about changes to the
       ledger state.

    Contracts will offer an HTTP interface with the following routes.

    * 1 route "initialise" to get the list of endpoints and interesting
      addresses at the start of the instance
    * 1 route "update" to advance the contract to the next state given some
      input (user interaction or blockchain)

-}
