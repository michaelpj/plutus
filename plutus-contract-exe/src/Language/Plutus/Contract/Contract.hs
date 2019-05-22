{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Plutus.Contract.Contract(
      Contract
    , done
    , emit
    , waiting
    , applyInput
    , applyInputs
    , drain
    , outputs
    , isDone
    , await
    , result
    , loopM
    ) where

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
        Pure f -> fmap f ca
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

done :: Contract t i a
done = Done

emit :: t -> Contract t i ()
emit t = Emit t (pure ())

waiting :: Contract t i i
waiting = Waiting pure

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
await a f = do
    emit a
    i <- waiting
    case f i of
        Nothing -> await a f
        Just i' -> pure i'
