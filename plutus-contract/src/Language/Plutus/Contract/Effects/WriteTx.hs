{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
module Language.Plutus.Contract.Effects.WriteTx where

import           Data.Row

import           Language.Plutus.Contract.Request        as Req
import           Language.Plutus.Contract.Rows.Instances (Event (..), Hooks (..))
import           Language.Plutus.Contract.Tx             (UnbalancedTx)


type TxReq = ("tx" .== [UnbalancedTx])
type TxResp = ("tx" .== ())
type TxPrompt ρ σ =
    ( HasType "tx" () ρ
    , HasType "tx" [UnbalancedTx] σ)

--  | Send an unbalanced transaction to the wallet.
writeTx :: UnbalancedTx -> Contract TxResp TxReq ()
writeTx t = mkRequest [t] Just

event
  :: forall ρ. (HasType "tx" () ρ, AllUniqueLabels ρ)
  => Event ρ
event = Event (IsJust #tx ())

transactions
  :: forall ρ.
  ( HasType "tx" [UnbalancedTx] ρ )
   => Hooks ρ
   -> [UnbalancedTx]
transactions (Hooks r) = r .! #tx
