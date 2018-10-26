-- | Basic functions for use in Plutus programs.
module Language.Plutus.Prelude where

-- this module does lots of weird stuff deliberately
{-# ANN module "HLint: ignore" #-}

{-# INLINABLE (&&) #-}
(&&) :: Bool -> Bool -> Bool
(&&) a b = if a then b else False

{-# INLINABLE (||) #-}
(||) :: Bool -> Bool -> Bool
(||) a b = if a then True else b

{-# INLINABLE not #-}
not :: Bool -> Bool
not a = if a then False else True
