{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusTx.These where

import           Language.Haskell.TH          (Q, TExp)

data These a b = This a | That b | These a b

these :: Q (TExp (a -> b -> (a -> b -> c) -> These a b -> c))
these = [||
    \a' b' f -> \case
        This a -> f a b'
        That b -> f a' b
        These a b -> f a b
    ||]

leftBiased :: Q (TExp (These a a -> a))
leftBiased =
    [|| \case
            This a -> a
            That a -> a
            These a _ -> a
    ||]

rightBiased :: Q (TExp (These a a -> a))
rightBiased =
    [|| \case
            This a -> a
            That a -> a
            These _ a -> a
    ||]

andDefinitely :: Q (TExp (Maybe a -> b -> These a b))
andDefinitely =
    [|| \ma b -> case ma of
            Just a -> These a b
            Nothing -> That b
    ||]

andMaybe :: Q (TExp (a -> Maybe b -> These a b))
andMaybe =
    [|| \a mb -> case mb of
            Just b -> These a b
            Nothing -> This a
    ||]
