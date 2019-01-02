{-# LANGUAGE FlexibleContexts #-}
module Language.PlutusIR.Optimizer (optimize) where

import           Language.PlutusIR
import           Language.PlutusIR.Optimizer.DeadCode
import           Language.PlutusIR.Optimizer.Merge

import qualified Language.PlutusCore                  as PLC
import qualified Language.PlutusCore.Name             as PLC

-- | Perform the "default" optimizations on a PIR program.
optimize
    :: (PLC.HasUnique (name a) PLC.TermUnique, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Term tyname name a
    -> Term tyname name a
-- Order of optimizations (currently fairly simple):
-- 1. Dead code removal (makes everything faster and less complex)
-- 2. Let merging
optimize = mergeLets . removeDeadBindings
