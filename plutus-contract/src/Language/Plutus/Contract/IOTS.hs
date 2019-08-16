{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Plutus.Contract.IOTS(
    IotsType(..)
  , schemaMap
  ) where

import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Proxy            (Proxy (..))
import           Data.Row
import           Data.Row.Internal     (Unconstrained1)
import qualified Data.Row.Records      as Records
import           Data.Sequence         (Seq)
import qualified Data.Sequence         as Seq
import           Data.Set              (Set)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           GHC.TypeLits          (symbolVal)
import           IOTS                  (IotsType (..), export)
import           Ledger.Crypto         (Signature)
import           Ledger.Scripts        (DataScript, RedeemerScript, ValidatorScript)
import           Ledger.Slot           (Slot)
import           Ledger.Tx             (Address, Tx, TxIn, TxInType, TxOut, TxOutOf, TxOutRefOf, TxOutType)


mksConst :: forall l a. (KnownSymbol l, IotsType a) => Label l -> (Const (Seq Text)) a
mksConst Label = Const (Seq.fromList [Text.pack $ symbolVal (Proxy @l), export (Proxy @a)])
--  (symbolVal (Proxy @l))

mkSchema :: forall ρ. (AllUniqueLabels ρ, Forall ρ IotsType) => Rec (Records.Map (Const (Seq Text)) ρ)
mkSchema = runIdentity (Records.fromLabelsMapA @IotsType @Identity @(Const (Seq Text)) @ρ (Identity . mksConst))

unSchema :: forall ρ. Forall ρ Unconstrained1 => Rec (Records.Map (Const (Seq Text)) ρ) -> Const (Seq Text) (Rec ρ)
unSchema = Records.sequence

schemaMap :: forall ρ. (AllUniqueLabels ρ, Forall ρ IotsType, Forall ρ Unconstrained1) => Const (Seq Text) (Rec ρ)
schemaMap = unSchema @ρ (mkSchema @ρ)

instance IotsType Address where
  iotsDefinition = iotsDefinition @String -- FIXME: Fix this instance and add to IOTS module

instance IotsType DataScript where
  iotsDefinition = iotsDefinition @String -- FIXME: Fix this instance and add to IOTS module

instance IotsType RedeemerScript where
  iotsDefinition = iotsDefinition @String -- FIXME: Fix this instance and add to IOTS module

instance IotsType ValidatorScript where
  iotsDefinition = iotsDefinition @String -- FIXME: Fix this instance and add to IOTS module

instance IotsType (TxOutRefOf a) where
  iotsDefinition = iotsDefinition @String -- FIXME: Fix this instance and add to IOTS module

instance IotsType (TxOutOf a) where
  iotsDefinition = iotsDefinition @String -- FIXME: Fix this instance and add to IOTS module

deriving instance IotsType TxOutType
deriving instance IotsType TxIn

instance IotsType (Set TxIn) where
  iotsDefinition = iotsDefinition @[TxIn] -- FIXME: Fix this instance and add to IOTS module

deriving instance IotsType TxInType
deriving instance IotsType Tx

instance IotsType Signature where
  iotsDefinition = iotsDefinition @String
