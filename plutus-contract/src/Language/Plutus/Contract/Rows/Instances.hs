{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DerivingVia             #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedLabels        #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Plutus.Contract.Rows.Instances(
      Hooks(..)
    , Event(..)
    , generalise
    ) where

import           Data.Aeson            (FromJSON, ToJSON, (.:))
import qualified Data.Aeson            as Aeson
import qualified Data.Aeson.Types      as Aeson
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Proxy            (Proxy (..))
import           Data.Row
import           Data.Row.Internal     (metamorph)
import qualified Data.Row.Records      as Records
import qualified Data.Row.Variants     as Variants
import           Data.Text             (Text)
import qualified Data.Text             as Text

newtype Event ρ = Event { unEvent :: Var ρ }

deriving instance Forall ρ Show => Show (Event ρ)
deriving instance Forall ρ Eq => Eq (Event ρ)

newtype Hooks ρ = Hooks { unHooks :: Rec ρ }

deriving instance Forall ρ Show => Show (Hooks ρ)

instance Forall ρ ToJSON => ToJSON (Hooks ρ) where
  toJSON = Aeson.object . Records.eraseWithLabels @ToJSON @ρ @Text @Aeson.Value Aeson.toJSON . unHooks

instance (AllUniqueLabels ρ, Forall ρ FromJSON) => FromJSON (Hooks ρ) where
  parseJSON vl = fmap Hooks $ Records.fromLabelsA @FromJSON @Aeson.Parser @ρ  (\lbl -> Aeson.withObject "Rec" (\obj -> obj .: (Text.pack $ show lbl) >>= Aeson.parseJSON) vl)

instance Forall ρ Semigroup => Semigroup (Hooks ρ) where
  (<>) = merge

instance (AllUniqueLabels ρ, Forall ρ Semigroup, Forall ρ Monoid) => Monoid (Hooks ρ) where
  mempty = Hooks (Records.default' @Monoid @ρ mempty)
  mappend = (<>)

generalise :: forall ρ ρ'. (AllUniqueLabels ρ', Forall ρ' Monoid, (ρ .// ρ') ~ ρ') => Hooks ρ -> Hooks ρ'
generalise (Hooks l) = Hooks $ l .// (Records.default' @Monoid @ρ' mempty)

merge :: forall ρ. Forall ρ Semigroup => Hooks ρ -> Hooks ρ -> Hooks ρ
merge (Hooks rec1) (Hooks rec2) = Hooks $ metamorph @_ @ρ @Semigroup @(Product Rec Rec) @Rec @Identity Proxy doNil doUncons doCons (Pair rec1 rec2)
  where
    doNil _ = empty
    doUncons l (Pair r1 r2) = (Identity $ r1 .! l <> r2 .! l, Pair (Records.unsafeRemove l r1) (Records.unsafeRemove l r2))
    doCons l (Identity v) r = Records.unsafeInjectFront l v r


instance (AllUniqueLabels ρ, Forall ρ FromJSON) => FromJSON (Event ρ) where
  parseJSON vl = fmap Event $ Variants.fromLabels @FromJSON @ρ @Aeson.Parser (\lbl -> Aeson.withObject "Var" (\obj -> do { tg <- obj .: "tag"; if tg == show lbl then (obj .: "value") >>= Aeson.parseJSON else fail "Wrong label" }) vl)

instance Forall ρ ToJSON => ToJSON (Event ρ) where
  toJSON (Event v) = Aeson.object [Variants.eraseWithLabels @ToJSON @ρ @Text @Aeson.Value Aeson.toJSON v]
