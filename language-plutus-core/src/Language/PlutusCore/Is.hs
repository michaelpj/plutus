{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
module Language.PlutusCore.Is (Is, embed, embed2) where

class Is a b where
  embed :: a -> b

instance Is a a where
  embed = id

embed2 :: forall a b c. (Is a b, Is b c) => a -> c
embed2 = (embed @b @c) . (embed @a @ b)
