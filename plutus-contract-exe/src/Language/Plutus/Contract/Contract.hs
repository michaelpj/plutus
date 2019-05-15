{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Plutus.Contract.Contract where

import           Control.Applicative (liftA2)
import           Control.Monad       ((>=>))
import           Data.Bifunctor      (first)
import           Data.List           (foldl')

data Contract t i a =
    Waiting (i -> Contract t i a)
    | Done
    | Emit t (Contract t i a) -- produce a 't' value
    | Pure a
    deriving (Functor)

-- The applicative instance parallelises the 'Waiting' operations
instance Applicative (Contract t i) where
    pure = Pure
    cf <*> ca = case cf of
        Done -> Done
        Emit t c -> Emit t (c <*> ca)
        Pure f -> case ca of
            Done       -> Done
            Emit t c   -> Emit t (Pure f <*> c)
            Pure a     -> Pure (f a)
            Waiting f' -> Waiting $ \i' -> fmap f (f' i')
        Waiting f ->
            case ca of
                Done -> Done
                Emit t c -> Emit t (Waiting f <*> c)
                Pure a' -> Waiting $ \i' -> fmap (\f' -> f' a') (f i')
                Waiting f' ->
                    Waiting $ \i' -> f i' <*> f' i'

-- The monad instance sequentialises the 'Waiting' operations
instance Monad (Contract t i) where
    c >>= f = case c of
        Waiting f' -> Waiting (f' >=> f)
        Done       -> Done
        Pure a     -> f a
        Emit t c'  -> Emit t (c' >>= f)

instance Semigroup a => Semigroup (Contract t i a) where
    (<>) = liftA2 (<>)

-- | Apply an input to a contract, collecting as much output data
--   't' as posible until the contract is blocked on inputs
applyInput
    :: Monoid t
    => i
    -> Contract t i a
    -> (t, Contract t i a)
applyInput ip = \case
    Waiting f -> drain (f ip)
    Done -> (mempty, Done)
    Pure a -> (mempty, Pure a)
    Emit t c -> first (t <>) (applyInput ip c)

applyInputs
    :: Monoid t
    => Contract t i a
    -> [i]
    -> ([t], Contract t i a)
applyInputs c = foldl' go (first return (drain c)) where
    go (ts, c') i = first (:ts) (applyInput i c')

drain :: Monoid t => Contract t i a -> (t, Contract t i a)
drain = \case
    Waiting f -> (mempty, Waiting f)
    Done -> (mempty, Done)
    Pure a -> (mempty, Pure a)
    Emit t c -> first (t <>) (drain c)

outputs :: Monoid t => Contract t i a -> t
outputs = fst . drain

result :: Monoid t => Contract t i a -> Maybe a
result = (\case { Pure b -> Just b; _ -> Nothing }) . snd . drain

isDone :: Monoid t => Contract t i a -> Bool
isDone = (\case { Done -> True; _ -> False }) . snd . drain

await :: t -> (i -> Maybe a) -> Contract t i a
await a f = Emit a $ Waiting $ maybe (await a f) Pure . f
