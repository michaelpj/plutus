{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Plutus.Contract.Request where

import qualified Control.Applicative                     as Applicative
import qualified Data.Aeson                              as Aeson
import           Data.Row
import           Data.Row.Internal                       (Subset, Unconstrained1)
import qualified Data.Row.Records                        as Records
import qualified Data.Row.Variants                       as Variants
import           Language.Plutus.Contract.Resumable
import           Language.Plutus.Contract.Rows.Instances (Event (..), Hooks (..), generalise)
import qualified Language.Plutus.Contract.Util           as Util

import           Prelude                                 hiding (return, (>>=))
import qualified Prelude

-- | @Contract ρ σ a@ is a contract that expects input events of type @ρ@ and produces
--   requests (describing the acceptable input) of type @σ@. The two type parameters
--   are 'Data.Row.Row' rows
--
type Contract (ρ :: Row *) (σ :: Row *) a = Resumable (Step (Maybe (Event ρ)) (Hooks σ)) a

mkRequest
  :: forall sReq sResp req resp a.
     ( KnownSymbol sReq
     , KnownSymbol sResp
     )
    => req
    -> (resp -> Maybe a)
    -> Contract (sResp .== resp) (sReq .== req) a
mkRequest out check = CStep (Step go) where
  upd = Left $ Hooks $ (Label @sReq) .==  out
  go Nothing = upd
  go (Just (Event rho)) = case trial rho (Label @sResp) of
    Left resp -> maybe upd Right (check resp)
    _         -> upd

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

joinBoth :: forall ρ₁ σ₁ ρ₂ σ₂ a b. Join ρ₁ σ₁ ρ₂ σ₂ => Contract ρ₁ σ₁ a -> Contract ρ₂ σ₂ b -> (Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) a, Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) b)
joinBoth l r = (mapStep (mapO s1 . mapI g1) l, mapStep (mapO s2 . mapI g2) r) where
  g1 s = s Prelude.>>= fmap Event . Variants.restrict . unEvent
  g2 s = s Prelude.>>= fmap Event . Variants.restrict . unEvent
  s1 = generalise @σ₁ @(σ₁ .\/ σ₂)
  s2 = generalise @σ₂ @(σ₁ .\/ σ₂)

cMap :: forall ρ σ a b. (a -> b) -> Contract ρ σ a -> Contract ρ σ b
cMap = CMap

cAp :: forall ρ₁ σ₁ ρ₂ σ₂ a b. ( Join ρ₁ σ₁ ρ₂ σ₂) => Contract ρ₁ σ₁ (a -> b) -> Contract ρ₂ σ₂ a -> Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) b
cAp l r = CAp l' r' where
  (l', r') = joinBoth @ρ₁ @σ₁ @ρ₂ @σ₂ @(a -> b) @a l r

cAlt :: forall ρ₁ σ₁ ρ₂ σ₂ a .
  ( Join ρ₁ σ₁ ρ₂ σ₂)
  => Contract ρ₁ σ₁ a
  -> Contract ρ₂ σ₂ a
  -> Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) a
cAlt l r = CAlt l' r' where
  (l', r') = joinBoth @ρ₁ @σ₁ @ρ₂ @σ₂ @a @a l r

cEmpty :: forall a. Contract Empty Empty a
cEmpty = CEmpty

cBind :: forall ρ₁ σ₁ ρ₂ σ₂ a b. (Join ρ₁ σ₁ ρ₂ σ₂) => Contract ρ₁ σ₁ a -> (a -> Contract ρ₂ σ₂ b) -> Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) b
cBind l f = CBind (mapStep (mapO s1 . mapI g1) l) (fmap (mapStep (mapO s2 . mapI g2)) f) where
  g1 s = s Prelude.>>= fmap Event . Variants.restrict . unEvent
  g2 s = s Prelude.>>= fmap Event . Variants.restrict . unEvent
  s1 = generalise @σ₁ @(σ₁ .\/ σ₂)
  s2 = generalise @σ₂ @(σ₁ .\/ σ₂)

(>>=) :: forall ρ₁ σ₁ ρ₂ σ₂ a b. ( Join ρ₁ σ₁ ρ₂ σ₂) => Contract ρ₁ σ₁ a -> (a -> Contract ρ₂ σ₂ b) -> Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) b
(>>=) = cBind

(>>) :: forall ρ₁ σ₁ ρ₂ σ₂ a b. ( Join ρ₁ σ₁ ρ₂ σ₂) => Contract ρ₁ σ₁ a -> Contract ρ₂ σ₂ b -> Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) b
l >> r = l >>= const r

select :: forall ρ₁ σ₁ ρ₂ σ₂ a. ( Join ρ₁ σ₁ ρ₂ σ₂) => Contract ρ₁ σ₁ a -> Contract ρ₂ σ₂ a -> Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) a
select l r = l' Applicative.<|> r' where
  (l', r') = joinBoth l r

(<|>) :: forall ρ₁ σ₁ ρ₂ σ₂ a. ( Join ρ₁ σ₁ ρ₂ σ₂) => Contract ρ₁ σ₁ a -> Contract ρ₂ σ₂ a -> Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) a
l <|> r = select l r

selectEither :: forall ρ₁ σ₁ ρ₂ σ₂ a b. ( Join ρ₁ σ₁ ρ₂ σ₂) => Contract ρ₁ σ₁ a -> Contract ρ₂ σ₂ b -> Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) (Either a b)
selectEither l r = Util.selectEither l' r' where
  (l', r') = joinBoth l r

both :: forall ρ₁ σ₁ ρ₂ σ₂ a b. ( Join ρ₁ σ₁ ρ₂ σ₂) => Contract ρ₁ σ₁ a -> Contract ρ₂ σ₂ b -> Contract (ρ₁ .\/ ρ₂) (σ₁ .\/ σ₂) (a, b)
both l r = Util.both l' r' where
  (l', r') = joinBoth l r

return :: forall a. a -> Contract Empty Empty a
return = CStep . Prelude.return

cStep :: forall ρ σ a. Step (Maybe (Event ρ)) (Hooks σ) a -> Contract ρ σ a
cStep = CStep

cJSONCheckpoint :: forall ρ σ a. (Aeson.FromJSON a, Aeson.ToJSON a) => Contract ρ σ a -> Contract ρ σ a
cJSONCheckpoint = CJSONCheckpoint

emptyRec :: forall ρ. (Forall ρ Monoid, AllUniqueLabels ρ) => Rec ρ
emptyRec = Records.default' @Monoid @ρ mempty

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _  = a
ifThenElse False _ b = b
