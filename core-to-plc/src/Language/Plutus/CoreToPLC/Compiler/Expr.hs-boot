{-# LANGUAGE FlexibleContexts  #-}

module Language.Plutus.CoreToPLC.Compiler.Expr (convExpr, convExprWithDefs, convDataConRef) where

import           Language.Plutus.CoreToPLC.Compiler.Types
import           Language.Plutus.CoreToPLC.PLCTypes

import qualified GhcPlugins                               as GHC

convDataConRef :: Converting m => GHC.DataCon -> m PLCTerm

convExpr :: Converting m => GHC.CoreExpr -> m PLCTerm

convExprWithDefs :: Converting m => GHC.CoreExpr -> m PLCTerm
