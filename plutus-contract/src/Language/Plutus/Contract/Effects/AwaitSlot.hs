{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Language.Plutus.Contract.Effects.AwaitSlot where

import           Data.Row
import           Data.Row.Internal                       (Subset, Unconstrained1)
import           Data.Semigroup
import           GHC.OverloadedLabels
import           Prelude                                 hiding (return, until, (>>=))

import           Language.Plutus.Contract.Request        as Req
import           Language.Plutus.Contract.Rows.Instances (Event (..), Hooks (..))
import           Language.Plutus.Contract.Util           (foldMaybe)

import           Ledger.Slot                             (Slot)

type SlotReq = ("slot" .== Maybe (Min Slot))
type SlotResp = ("slot" .== Slot)

-- | A contract that waits until the slot is reached, then returns the
--   current slot.
awaitSlot :: Slot -> Contract SlotResp SlotReq Slot
awaitSlot sl =
  let s = Just $ Min sl in
  mkRequest s $ \sl' -> if sl' >= sl then Just sl' else Nothing

event
    :: forall ρ.
    ( HasType "slot" Slot ρ
    , AllUniqueLabels ρ)
    => Slot
    -> Event ρ
event = Event . IsJust #slot

nextSlot
    :: forall ρ.
    ( HasType "slot" (Maybe (Min Slot)) ρ)
    => Hooks ρ
    -> Maybe Slot
nextSlot (Hooks r) = fmap getMin (r .! #slot)


-- | Run a contract until the given slot has been reached.
until
  :: forall ρ σ a.
      ( Join SlotResp SlotReq ρ σ)
  => Contract ρ σ a
  -> Slot
  -> Contract (SlotResp .\/ ρ) (SlotReq .\/ σ) (Maybe a)
until c sl =
  Req.cMap (either (const Nothing) Just) (Req.selectEither (awaitSlot sl) c)

-- | Run a contract when the given slot has been reached.
when
  :: forall ρ σ a.
  ( Join SlotResp SlotReq ρ σ )
  => Slot
  -> Contract ρ σ a
  -> Contract (SlotResp .\/ ρ) (SlotReq .\/ σ) a
when s c = awaitSlot s Req.>> c

-- | Run a contract until the given slot has been reached.
--   @timeout = flip until@
timeout
  :: forall ρ σ a.
  ( Join SlotResp SlotReq ρ σ )
  => Slot
  -> Contract ρ σ a
  -> Contract (SlotResp .\/ ρ) (SlotReq .\/ σ) (Maybe a)
timeout = flip until

-- | Wait until the first slot is reached, then run the contract until
--   the second slot is reached.
between
  :: forall ρ σ a.
  ( Join SlotResp SlotReq ρ σ
  , (SlotReq .// (SlotReq .\/ (SlotReq .\/ σ))) ~ (SlotReq .\/ (SlotReq .\/ σ))
  , ((SlotReq .\/ σ) .// (SlotReq .\/ (SlotReq .\/ σ))) ~ (SlotReq .\/ (SlotReq .\/ σ))
  , (SlotReq .\/ (SlotReq .\/ σ)) ~ (SlotReq .\/ σ)
  , (SlotResp .\/ (SlotResp .\/ ρ)) ~ (SlotResp .\/ ρ)
  , AllUniqueLabels (SlotResp .\/ ρ)
  , Subset (SlotReq .\/ σ) (SlotReq .\/ σ)
  , Subset (SlotResp .\/ ρ) (SlotResp .\/ ρ)
  , Forall (SlotResp .\/ ρ) Unconstrained1
  , Forall (SlotReq .\/ σ) Unconstrained1
  )
  => Slot
  -> Slot
  -> Contract ρ σ a
  -> Contract (SlotResp .\/ ρ) (SlotReq .\/ σ) (Maybe a)
between a b = timeout b . when a

-- The constraints are a bit annoying :(

-- | Repeatedly run a contract until the slot is reached, then
--   return the last result.
collectUntil
  :: forall ρ σ a b.
  ( Join SlotResp SlotReq ρ σ )
  => (a -> b -> b)
  -> b
  -> Contract ρ σ a
  -> Slot
  -> Contract (SlotResp .\/ ρ) (SlotReq .\/ σ) b
collectUntil f b con s = foldMaybe f b (until con s)
