{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Language.Plutus.Contract.Effects.WriteTx where

import           Data.Row

import           Language.Plutus.Contract.Request as Req
import           Language.Plutus.Contract.Tx      (UnbalancedTx)


type TxReq = ("tx" .== [UnbalancedTx])
type TxResp = ("tx" .== ())

--  | Send an unbalanced transaction to the wallet.
writeTx :: UnbalancedTx -> Contract TxResp TxReq ()
writeTx t = mkRequest [t] Just
