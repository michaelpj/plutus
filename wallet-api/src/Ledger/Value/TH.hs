{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE LambdaCase         #-}
{-# OPTIONS_GHC -Wno-orphans    #-}
module Ledger.Value.TH(
      Value(..)
    , CurrencySymbol
    , currencySymbol
    , singleton
    , valueOf
    , scale
      -- * Constants
    , zero
      -- * Num operations
    , plus
    , minus
    , multiply
    , negate
    , geq
    , gt
    , leq
    , lt
    , eq
      -- * Etc.
    , isZero
    ) where

import           Codec.Serialise.Class        (Serialise)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Swagger.Internal.Schema (ToSchema(..), genericDeclareNamedSchemaUnrestricted)
import           Data.Swagger.SchemaOptions   (defaultSchemaOptions)
import           GHC.Generics                 (Generic)
import           Language.PlutusTx.Lift       (makeLift)
import qualified Language.PlutusTx.Prelude    as P
import qualified Language.PlutusTx.Map.TH        as Map
import           Language.PlutusTx.These
import           Language.Haskell.TH          (Q, TExp)
import           Prelude                      hiding (all, lookup, negate)

data CurrencySymbol = CurrencySymbol Int
  deriving (Eq, Ord, Show)
  deriving stock (Generic)
  deriving anyclass (ToSchema, ToJSON, FromJSON, Serialise)

makeLift ''CurrencySymbol

curCmp :: Q (TExp (CurrencySymbol -> CurrencySymbol -> Ordering))
curCmp = [|| \(CurrencySymbol i) (CurrencySymbol j) -> if i < j then LT else if i == j then EQ else GT ||]

currencySymbol :: Q (TExp (Int -> CurrencySymbol))
currencySymbol = [|| CurrencySymbol ||]

-- | Cryptocurrency value
--   See note [Currencies]
newtype Value = Value { getValue :: Map.Map CurrencySymbol Int }
    deriving (Show)
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving anyclass (ToSchema)
    deriving newtype (Serialise)

deriving instance (ToJSON (Map.Color))
deriving instance (FromJSON (Map.Color))
deriving instance (ToSchema (Map.Color))

instance (ToSchema k, ToSchema v) => (ToSchema (Map.Map k v)) where
    -- I got an error message telling me to do this when I tried to derive the instance, I
    -- don't entirely understand why, but this may be wrong.
    declareNamedSchema proxy = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions proxy
deriving instance (ToJSON k, ToJSON v) => (ToJSON (Map.Map k v))
deriving instance (FromJSON k, FromJSON v) => (FromJSON (Map.Map k v))

makeLift ''Value

{- note [Currencies]

The 'Value' type represents a collection of amounts of different currencies.

We can think of 'Value' as a vector space whose dimensions are
currencies. At the moment there is only a single currency (Ada), so 'Value'
contains one-dimensional vectors. When currency-creating transactions are
implemented, this will change and the definition of 'Value' will change to a
'Map Currency Int', effectively a vector with infinitely many dimensions whose
non-zero values are recorded in the map.

To create a value of 'Value', we need to specifiy a currency. This can be done
using 'Ledger.Ada.adaValueOf'. To get the ada dimension of 'Value' we use
'Ledger.Ada.fromValue'. Plutus contract authors will be able to define modules
similar to 'Ledger.Ada' for their own currencies.

-}

-- | How much of a given currency is in a 'Value'
valueOf :: Q (TExp (Value -> CurrencySymbol -> Int))
valueOf = [||
  \(Value xs) cur ->
      case $$(Map.lookup) $$curCmp cur xs of
        Nothing -> 0 :: Int
        Just i  -> i
   ||]

singleton :: Q (TExp (CurrencySymbol -> Int -> Value))
singleton = [|| \sym i -> Value $ $$(Map.insert) $$curCmp sym i $ $$(Map.nil) ||]

scale :: Q (TExp (Int -> Value -> Value))
scale = [|| \i (Value xs) -> Value ($$(Map.map) ($$(P.multiply) i) xs) ||]

-- Num operations

plus :: Q (TExp (Value -> Value -> Value))
plus = [|| \(Value v1) (Value v2) -> Value $ $$(Map.unionWith) $$curCmp $$(P.plus) v1 v2||]

negate :: Q (TExp (Value -> Value))
negate = [|| $$(scale) (-1) ||]

minus :: Q (TExp (Value -> Value -> Value))
minus = [|| \(Value v1) (Value v2) -> Value $ $$(Map.unionWith) $$curCmp $$(P.minus) v1 v2 ||]

multiply :: Q (TExp (Value -> Value -> Value))
multiply = [|| \(Value v1) (Value v2) -> Value $ $$(Map.unionWith) $$curCmp $$(P.multiply) v1 v2 ||]

zero :: Q (TExp Value)
zero = [|| Value $$(Map.nil) ||]

isZero :: Q (TExp (Value -> Bool))
isZero = [|| \(Value xs) -> $$(P.all) (\i -> $$(P.eq) 0 i) ($$(Map.values) xs) ||]

-- | Lifts a binary predicate on 'Int' pointwise to a binary predicate on 'Value',
-- using 0 as a default value when either side does not contain a mapping for a key.
liftBinop :: Q (TExp ((Int -> Int -> Bool) -> Value -> Value -> Bool))
liftBinop = [||
  \op (Value ls) (Value rs) ->
    let p = \case
            This i -> i `op` 0
            That j -> 0 `op` j
            These i j -> i `op` j
        -- This uses 'unionThese', which is less efficient than 'unionWith'. But
        -- there's no escaping that: in this situation we *do* need to look at every
        -- element in both maps.
        union = $$(Map.unionThese) $$curCmp ls rs
    in $$(P.all) p ($$(Map.values) union) ||]

geq :: Q (TExp (Value -> Value -> Bool))
geq = [|| $$liftBinop (>=) ||]

gt :: Q (TExp (Value -> Value -> Bool))
gt = [|| $$liftBinop (>) ||]

leq :: Q (TExp (Value -> Value -> Bool))
leq = [|| $$liftBinop (<=) ||]

lt :: Q (TExp (Value -> Value -> Bool))
lt = [|| $$liftBinop (<) ||]

eq :: Q (TExp (Value -> Value -> Bool))
eq = [|| $$liftBinop (==) ||]
