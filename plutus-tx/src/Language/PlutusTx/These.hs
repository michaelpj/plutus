{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusTx.These where

import           Language.Haskell.TH          (Q, TExp)

-- | Represents either a single value of one type, or a single value of
-- the other type, or one of each.
--
-- Local version of 'Data.These', so we can provide TH functions and avoid
-- the dependency on an external type in generated code.
data These a b = This a | That b | These a b

these :: Q (TExp ((a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c))
these = [||
    \f g h -> \case
        This a -> f a
        That b -> g b
        These a b -> h a b
    ||]

mergeThese :: Q (TExp ((a -> a -> a) -> These a a -> a))
mergeThese =
    [|| \f -> \case
            This a -> a
            That a -> a
            These a a' -> f a a'
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
