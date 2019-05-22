{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Plutus.Contract.Contract(
      Contract
    , applyInput
    , applyInputs
    , drain
    , loopM
    , awaitUntil
    ) where

import           Control.Applicative  (liftA2)
import           Control.Monad        ((>=>))
import           Control.Monad.Writer
import           Control.Monad.Morph

import           Data.Functor.Identity

import Pipes

type Contract t i a = Pipe i t Identity a

-- https://hackage.haskell.org/package/extra-1.6.15/docs/src/Control.Monad.Extra.html#loopM

-- | A monadic version of 'loop', where the predicate returns 'Left' as a seed for the next loop
--   or 'Right' to abort the loop.
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
    res <- act x
    case res of
        Left x' -> loopM act x'
        Right v -> return v

-- | Apply an input to a contract, collecting as much output data
--   't' as posible until the contract is blocked on inputs
applyInput
    :: Contract t i ()
    -> i
    -> Producer t Identity ()
applyInput c ip = yield ip >-> c

applyInputs
    :: Contract t i ()
    -> [i]
    -> Producer t Identity ()
applyInputs c ip = each ip >-> c

drain :: Monoid t => Producer t Identity a -> Effect (Writer t) a
drain c = for (hoist generalize c) $ \it -> tell it

awaitUntil :: (i -> Maybe a) -> Contract t i a
awaitUntil f = do
    i <- await
    case f i of
        Nothing -> awaitUntil f
        Just i' -> pure i'
