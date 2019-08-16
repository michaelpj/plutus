{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module Language.Plutus.Contract(
      Contract
    , ContractTypes
    , both
    , selectEither
    , select
    , (<|>)
    -- * Dealing with time
    , SlotPrompt
    , awaitSlot
    , until
    , when
    , timeout
    , between
    , collectUntil
    -- * Endpoints
    , HasEndpoint
    , endpoint
    -- * Transactions
    , TxPrompt
    , writeTx
    -- * Blockchain events
    , AddressPrompt
    , nextTransactionAt
    , watchAddressUntil
    , fundsAtAddressGt
    -- * Transactions
    , module Tx
    -- * Operators for RebindableSyntax
    , (>>=)
    , (>>)
    , return
    , ifThenElse
    -- * HasType
    , HasType
    ) where

import           Data.Row                                        (AllUniqueLabels, Forall, HasType)

import           Language.Plutus.Contract.Effects.AwaitSlot
import           Language.Plutus.Contract.Effects.ExposeEndpoint
import           Language.Plutus.Contract.Effects.WatchAddress
import           Language.Plutus.Contract.Effects.WriteTx

import           Language.Plutus.Contract.Request                (Contract, both, ifThenElse, return, select,
                                                                  selectEither, (<|>), (>>), (>>=))
import           Language.Plutus.Contract.Tx                     as Tx

import           Prelude                                         hiding (return, until, (>>), (>>=))

type ContractTypes ρ σ =
    ( AllUniqueLabels ρ
    , AllUniqueLabels σ
    , Forall σ Monoid
    , Forall σ Semigroup
    )
