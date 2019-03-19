{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds           #-}
module TH.TestTH where

import           Language.Haskell.TH
import           Language.PlutusTx.Prelude

import           Language.PlutusTx.Code
import           Language.PlutusTx.TH
import qualified Language.PlutusTx.Builtins as Builtins
import           Language.PlutusTx.Prelude
import           Language.PlutusTx.Evaluation

{-# ANN module "HLint: ignore" #-}

power :: Int -> Q (TExp (Int -> Int))
power n =
    if n <= 0 then
        [|| \ _ -> (1::Int) ||]
    else if even n then
        [|| \(x::Int) -> let y = $$(power ($$divide n (2::Int))) x in $$multiply y y ||]
    else
        [|| \(x::Int) -> $$multiply x ($$(power ($$minus n (1::Int))) x) ||]

andTH :: Q (TExp (Bool -> Bool -> Bool))
andTH = [||\(a :: Bool) -> \(b::Bool) -> if a then if b then True else False else False||]

{-# NOINLINE andExternal #-}
andExternal :: Bool -> Bool -> Bool
andExternal = \(a :: Bool) -> \(b::Bool) -> if a then b else False
