{-# LANGUAGE ScopedTypeVariables #-}
module Plugin.Lib where

import           Language.Haskell.TH
import           Language.PlutusTx.Prelude

import           Language.PlutusTx.Code
import           Language.PlutusTx.TH
import qualified Language.PlutusTx.Builtins as Builtins
import           Language.PlutusTx.Prelude
import           Language.PlutusTx.Evaluation

{-# ANN module "HLint: ignore" #-}

-- This is here for the Plugin spec, but we're testing using things from a different module
andExternal :: Bool -> Bool -> Bool
andExternal = \(a :: Bool) -> \(b::Bool) -> if a then b else False
