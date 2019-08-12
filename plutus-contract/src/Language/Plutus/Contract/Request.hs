{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Plutus.Contract.Request where

import           Data.Aeson                         (FromJSON, ToJSON, (.:))
import qualified Data.Aeson                         as Aeson
import qualified Data.Aeson.Types                   as Aeson
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Proxy                         (Proxy (..))
import           Data.Row
import           Data.Row.Internal                  (Subset, Unconstrained1)
import qualified Data.Row.Records                   as Records
import qualified Data.Row.Variants                  as Variants
import           Data.Semigroup
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import           GHC.TypeLits                       (Symbol, symbolVal)
import           Language.Plutus.Contract.Resumable
import           Ledger.Slot                        (Slot)
import           Ledger.Tx                          (Address, Tx)

import           Prelude                            hiding (return, (>>), (>>=))
import qualified Prelude

-- | @Con ρ σ a@ is a contract that expects input events of type @ρ@ and produces
--   requests (describing the acceptable input) of type @σ@. The two type parameters
--   are 'Data.Row.Row' rows
--
type Con (ρ :: Row *) (σ :: Row *) a = Resumable (Step (Maybe (Var ρ)) (Endo (Rec σ))) a

awaitSlot :: Slot -> Con ("slot" .== Slot) ("slot" .== Maybe (Min Slot)) Slot
awaitSlot sl = CStep (Step go) where
  o = Just (Min sl)
  upd = Left $ Endo $ \r -> Records.update #slot (r .! #slot <> o) r
  go Nothing = upd
  go (Just rho) = case trial rho #slot of
    Left sl'
      | sl' >= sl -> Right sl'
    _ -> upd

watchAddress :: Address -> Con ("tx" .== Tx) ("address" .== Set Address) Tx
watchAddress addr = CStep (Step go) where
  upd = Left $ Endo $ \r -> Records.update #address (r .! #address <> Set.singleton addr) r
  go Nothing = upd
  go (Just rho) = case trial rho #tx of
    Left tx -> Right tx
    _       -> upd

newtype EndpointDescription = EndpointDescription String
    deriving (Eq, Ord)

exposeEndpoint :: forall (s :: Symbol) a. KnownSymbol s => Con (s .== a) (s .== Set EndpointDescription) a
exposeEndpoint = CStep (Step go) where
  upd = Left $ Endo $ \r -> Records.update (Label @s) (r .! Label @s <> Set.singleton (EndpointDescription $ symbolVal (Proxy @s))) r
  go Nothing = upd
  go (Just rho) = case trial rho (Label @s) of
    Left a -> Right a
    _      -> upd

type Join ρ₁ σ₁ ρ₂ σ₂ =
  ( AllUniqueLabels ρ₁
  , AllUniqueLabels ρ₂
  , AllUniqueLabels (σ₁ .\/ σ₂)
  , Forall (σ₁ .\/ σ₂) Monoid
  , Subset σ₁ (σ₁ .\/ σ₂)
  , Subset σ₂ (σ₁ .\/ σ₂)
  , (σ₁ .// (σ₁ .\/ σ₂)) ~ (σ₁ .\/ σ₂)
  , (σ₂ .// (σ₁ .\/ σ₂)) ~ (σ₁ .\/ σ₂)
  , Forall ρ₁ Unconstrained1
  , Forall σ₁ Unconstrained1
  , Forall ρ₂ Unconstrained1
  , Forall σ₂ Unconstrained1
  , Subset ρ₁ (ρ₁ .\/ ρ₂)
  , Subset ρ₂ (ρ₁ .\/ ρ₂))

joinBoth :: forall ρ₁ σ₁ ρ₂ σ₂ a b. Join ρ₁ σ₁ ρ₂ σ₂ => Con ρ₁ σ₁ a -> Con ρ₂ σ₂ b -> (Con (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) a, Con (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) b)
joinBoth l r = (mapStep (mapO s1 . mapI g1) l, mapStep (mapO s2 . mapI g2) r) where
  g1 s = s Prelude.>>= Variants.restrict
  g2 s = s Prelude.>>= Variants.restrict
  s1 (Endo f) = Endo $ \b -> (\b' -> b' .// emptyRec @(σ₁ .\/ σ₂)) $ f $ Records.restrict b
  s2 (Endo f) = Endo $ \b -> (\b' -> b' .// emptyRec @(σ₁ .\/ σ₂)) $ f $ Records.restrict b

cMap :: forall ρ σ a b. (a -> b) -> Con ρ σ a -> Con ρ σ b
cMap = CMap

cAp :: forall ρ₁ σ₁ ρ₂ σ₂ a b. ( Join ρ₁ σ₁ ρ₂ σ₂) => Con ρ₁ σ₁ (a -> b) -> Con ρ₂ σ₂ a -> Con (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) b
cAp l r = CAp l' r' where
  (l', r') = joinBoth @ρ₁ @σ₁ @ρ₂ @σ₂ @(a -> b) @a l r

cAlt :: forall ρ₁ σ₁ ρ₂ σ₂ a .
  ( Join ρ₁ σ₁ ρ₂ σ₂)
  => Con ρ₁ σ₁ a
  -> Con ρ₂ σ₂ a
  -> Con (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) a
cAlt l r = CAlt l' r' where
  (l', r') = joinBoth @ρ₁ @σ₁ @ρ₂ @σ₂ @a @a l r

cEmpty :: forall ρ σ a. Con ρ σ a
cEmpty = CEmpty

cBind :: forall ρ₁ σ₁ ρ₂ σ₂ a b. (Join ρ₁ σ₁ ρ₂ σ₂) => Con ρ₁ σ₁ a -> (a -> Con ρ₂ σ₂ b) -> Con (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) b
cBind l f = CBind (mapStep (mapO s1 . mapI g1) l) (fmap (mapStep (mapO s2 . mapI g2)) f) where
  g1 s = s Prelude.>>= Variants.restrict
  g2 s = s Prelude.>>= Variants.restrict
  s1 (Endo f') = Endo $ \b -> (\b' -> b' .// emptyRec @(σ₁ .\/ σ₂)) $ f' $ Records.restrict b
  s2 (Endo f') = Endo $ \b -> (\b' -> b' .// emptyRec @(σ₁ .\/ σ₂)) $ f' $ Records.restrict b

(>>=) :: forall ρ₁ σ₁ ρ₂ σ₂ a b. ( Join ρ₁ σ₁ ρ₂ σ₂) => Con ρ₁ σ₁ a -> (a -> Con ρ₂ σ₂ b) -> Con (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) b
(>>=) = cBind

(>>) :: forall ρ₁ σ₁ ρ₂ σ₂ a b. ( Join ρ₁ σ₁ ρ₂ σ₂) => Con ρ₁ σ₁ a -> Con ρ₂ σ₂ b -> Con (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) b
l >> r = l >>= const r

return :: forall a. a -> Con Empty Empty a
return = CStep . Prelude.return

cStep :: forall ρ σ a. Step (Maybe (Var ρ)) (Endo (Rec σ)) a -> Con ρ σ a
cStep = CStep

cJSONCheckpoint :: forall ρ σ a. (Aeson.FromJSON a, Aeson.ToJSON a) => Con ρ σ a -> Con ρ σ a
cJSONCheckpoint = CJSONCheckpoint

newtype JsonRow v ρ = JsonRow { unJsonRow :: v ρ }

instance Forall ρ ToJSON => ToJSON (JsonRow Rec ρ) where
  toJSON = Aeson.object . Records.eraseWithLabels @ToJSON @ρ @Text @Aeson.Value Aeson.toJSON . unJsonRow

instance (AllUniqueLabels ρ, Forall ρ FromJSON) => FromJSON (JsonRow Var ρ) where
  parseJSON vl = fmap JsonRow (Variants.fromLabels @FromJSON @ρ @Aeson.Parser (\lbl -> Aeson.withObject "Var" (\obj -> do { tg <- obj .: "tag"; if tg == show lbl then (obj .: "value") Prelude.>>= Aeson.parseJSON else fail "Wrong label" }) vl))

emptyRec :: forall ρ. (Forall ρ Monoid, AllUniqueLabels ρ) => Rec ρ
emptyRec = Records.default' @Monoid @ρ mempty

newtype Schema = Schema String
  deriving Show

class HasSchema a where
  schema :: Proxy a -> Schema

instance HasSchema Int where
  schema _ = Schema "Int"

instance HasSchema String where
  schema _ = Schema "String"

mksConst :: forall l a. (KnownSymbol l, HasSchema a) => Label l -> (Const [(String, Schema)]) a
mksConst Label = Const [(symbolVal (Proxy @l), schema (Proxy @a))]

mkSchema :: forall ρ. (AllUniqueLabels ρ, Forall ρ HasSchema) => Rec (Records.Map (Const [(String, Schema)]) ρ)
mkSchema = runIdentity (Records.fromLabelsMapA @HasSchema @Identity @(Const [(String, Schema)]) @ρ (Identity . mksConst))

unSchema :: forall ρ. Forall ρ Unconstrained1 => Rec (Records.Map (Const [(String, Schema)]) ρ) -> Const [(String, Schema)] (Rec ρ)
unSchema = Records.sequence

mkS :: forall ρ. (AllUniqueLabels ρ, Forall ρ HasSchema, Forall ρ Unconstrained1) => Const [(String, Schema)] (Rec ρ)
mkS = unSchema @ρ (mkSchema @ρ)

req :: IO ()
req = do
  let Const s = mkS @("a" .== Int .\/ "b" .== String)
  print s
