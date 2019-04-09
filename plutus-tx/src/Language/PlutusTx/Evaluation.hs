module Language.PlutusTx.Evaluation (evalCek, evalCekTrace) where

import           Language.PlutusCore
import           Language.PlutusCore.Constant
import           Language.PlutusCore.Constant.Dynamic
import           Language.PlutusCore.Interpreter.CekMachine

import           Control.Exception

import           System.IO.Unsafe

stringBuiltins :: DynamicBuiltinNameMeanings
stringBuiltins =
    insertDynamicBuiltinNameDefinition dynamicCharToStringDefinition $ insertDynamicBuiltinNameDefinition dynamicAppendDefinition mempty

-- | Evaluate a program in the CEK machine with the usual string dynamic builtins.
evalCek
    :: GasThreshold
    -> Program TyName Name ()
    -> EvaluationResultDef
evalCek threshold = evalCekProgram threshold stringBuiltins

-- TODO: pretty sure we shouldn't need the unsafePerformIOs here, we should expose a pure interface even if it has IO hacks under the hood

-- | Evaluate a program in the CEK machine with the usual string dynamic builtins and tracing, additionally
-- returning the trace output.
evalCekTrace
    :: GasThreshold
    -> Program TyName Name ()
    -> ([String], EvaluationResultDef)
evalCekTrace threshold p =
    unsafePerformIO $ withEmit $ \emit -> do
        let logName       = dynamicTraceName
            logDefinition = dynamicCallAssign logName emit
            env  = insertDynamicBuiltinNameDefinition logDefinition stringBuiltins
        evaluate $ evalCekProgram threshold env p
