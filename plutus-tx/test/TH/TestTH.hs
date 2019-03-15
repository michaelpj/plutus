{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module TH.TestTH where

import           Language.Haskell.TH
import           Language.PlutusTx.Plugin   (hoistFun)

{-# ANN module "HLint: ignore" #-}

power :: Int -> TExpQ (Int -> Int)
power n =
    if n <= 0 then
        [|| \ _ -> (1::Int) ||]
    else if even n then
        [|| \(x::Int) -> let y = $$(power (n `div` (2::Int))) x in y * y ||]
    else
        [|| \(x::Int) -> x * ($$(power (n - (1::Int))) x) ||]

andTH :: TExpQ (Bool -> Bool -> Bool)
andTH = [||\(a :: Bool) -> \(b::Bool) -> if a then if b then True else False else False||]

addOne :: TExpQ (Int -> Int)
addOne = [|| hoistFun addOne (\(x::Int) -> x + 1) ||]
