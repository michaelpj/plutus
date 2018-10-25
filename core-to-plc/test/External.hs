{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS -fplugin Language.Plutus.CoreToPLC.Plugin -fplugin-opt Language.Plutus.CoreToPLC.Plugin:defer-errors #-}
module External where
import           Language.Plutus.CoreToPLC.Plugin

{-# INLINABLE myAnd #-}
myAnd :: Bool -> Bool -> Bool
myAnd x y = myNand (myNand x y) (myNand x y)

-- I can't persuade GHC to have an unfolding for this and *not* inline it, despite
-- putting it in a separate module, fiddling with optimization settings etc.
{-# INLINABLE myNand #-}
myNand :: Bool -> Bool -> Bool
myNand x y = if x then False else if y then False else True

externalNand :: PlcCode
externalNand = plc @"externalNand" (\(x::Bool) (y::Bool) -> myNand x y)
