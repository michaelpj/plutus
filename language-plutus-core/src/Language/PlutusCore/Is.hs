{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Language.PlutusCore.Is (Is, embed) where

class Is a b where
  embed :: a -> b

instance Is a a where
  embed = id
