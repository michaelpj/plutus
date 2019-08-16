{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
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
module Language.Plutus.Contract.Schema(
  Schema(..)
  , HasSchema(..)
  , JsonRow(..)
  , schemaMap
  , emptyRec
  ) where

import           Data.Aeson            (FromJSON, ToJSON, (.:))
import qualified Data.Aeson            as Aeson
import qualified Data.Aeson.Types      as Aeson
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Proxy            (Proxy (..))
import           Data.Row
import           Data.Row.Internal     (Unconstrained1)
import qualified Data.Row.Records      as Records
import qualified Data.Row.Variants     as Variants
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           GHC.TypeLits          (symbolVal)

newtype JsonRow v ρ = JsonRow { unJsonRow :: v ρ }

instance Forall ρ ToJSON => ToJSON (JsonRow Rec ρ) where
  toJSON = Aeson.object . Records.eraseWithLabels @ToJSON @ρ @Text @Aeson.Value Aeson.toJSON . unJsonRow

instance (AllUniqueLabels ρ, Forall ρ FromJSON) => FromJSON (JsonRow Var ρ) where
  parseJSON vl = fmap JsonRow (Variants.fromLabels @FromJSON @ρ @Aeson.Parser (\lbl -> Aeson.withObject "Var" (\obj -> do { tg <- obj .: "tag"; if tg == show lbl then (obj .: "value") >>= Aeson.parseJSON else fail "Wrong label" }) vl))

emptyRec :: forall ρ. (Forall ρ Monoid, AllUniqueLabels ρ) => Rec ρ
emptyRec = Records.default' @Monoid @ρ mempty

newtype Schema = Schema String -- Dummy schema TODO: replace with real schema
  deriving (Eq, Ord, Show, Generic)

class HasSchema a where
  schema :: Proxy a -> Schema

instance HasSchema Int where
  schema _ = Schema "Int"

instance HasSchema String where
  schema _ = Schema "String"

mksConst :: forall l a. (KnownSymbol l, HasSchema a) => Label l -> (Const (Map String Schema)) a
mksConst Label = Const (Map.singleton (symbolVal (Proxy @l)) (schema (Proxy @a)))

mkSchema :: forall ρ. (AllUniqueLabels ρ, Forall ρ HasSchema) => Rec (Records.Map (Const (Map String Schema)) ρ)
mkSchema = runIdentity (Records.fromLabelsMapA @HasSchema @Identity @(Const (Map String Schema)) @ρ (Identity . mksConst))

unSchema :: forall ρ. Forall ρ Unconstrained1 => Rec (Records.Map (Const (Map String Schema)) ρ) -> Const (Map String Schema) (Rec ρ)
unSchema = Records.sequence

schemaMap :: forall ρ. (AllUniqueLabels ρ, Forall ρ HasSchema, Forall ρ Unconstrained1) => Const (Map String Schema) (Rec ρ)
schemaMap = unSchema @ρ (mkSchema @ρ)
