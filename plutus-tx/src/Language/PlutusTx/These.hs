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
