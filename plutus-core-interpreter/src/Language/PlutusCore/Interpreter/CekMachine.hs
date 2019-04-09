-- | The CEK machine.
-- Rules are the same as for the CK machine from "Language.PlutusCore.Evaluation.CkMachine",
-- except we do not use substitution and use environments instead.
-- The CEK machine relies on variables having non-equal 'Unique's whenever they have non-equal
-- string names. I.e. 'Unique's are used instead of string names. This is for efficiency reasons.
-- The type checker pass is a prerequisite.
-- Feeding ill-typed terms to the CEK machine will likely result in a 'MachineException'.
-- The CEK machine generates booleans along the way which might contain globally non-unique 'Unique's.
-- This is not a problem as the CEK machines handles name capture by design.
-- Dynamic extensions to the set of built-ins are allowed.
-- In case an unknown dynamic built-in is encountered, an 'UnknownDynamicBuiltinNameError' is returned
-- (wrapped in 'OtherMachineError').

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PlutusCore.Interpreter.CekMachine
    ( CekMachineException
    , EvaluationResult (..)
    , EvaluationResultDef
    , evalCekTermCatch
    , runCekTermCatch
    , evalCekTerm
    , runCekTerm
    , evalCekProgram
    , runCekProgram
    , readKnownCek
    ) where

import           Language.PlutusCore
import           Language.PlutusCore.Constant
import           Language.PlutusCore.Evaluation.Costs
import           Language.PlutusCore.Evaluation.MachineException
import           Language.PlutusCore.Evaluation.Stats
import           Language.PlutusCore.Name
import           Language.PlutusCore.View
import           PlutusPrelude                                   hiding (hoist)

import           Control.Lens                                    hiding (Context)
import           Control.Lens.TH                                 (makeLenses)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                                        as Map
import           Data.Tuple

type Plain f = f TyName Name ()

-- | The CEK machine-specific 'MachineException'.
type CekMachineException = MachineException UnknownDynamicBuiltinNameError

-- | A 'Value' packed together with the environment it's defined in.
data Closure = Closure
    { _closureVarEnv :: VarEnv
    , _closureValue  :: Plain Value
    }

-- | Variable environments used by the CEK machine.
-- Each row is a mapping from the 'Unique' representing a variable to a 'Closure'.
type VarEnv = UniqueMap TermUnique Closure

-- | The environment the CEK machine runs in.
data CekEnv = CekEnv
    { _cekEnvMeans        :: DynamicBuiltinNameMeanings
    , _cekEnvVarEnv       :: VarEnv
    , _cekEnvCostModel    :: CostModel (Plain Term)
    , _cekEnvGasThreshold :: GasThreshold
    }

makeLenses ''CekEnv

data CekState = CekState
    { _cekStateStats    :: Stats
    , _cekStateGasUsage :: Gas
    }

makeLenses ''CekState

emptyState :: CekState
emptyState = CekState mempty mempty

checkGasAt :: Plain Term -> CekM ()
checkGasAt term = do
    usage <- gets (view cekStateGasUsage)
    threshold <- asks (view cekEnvGasThreshold)
    unless (withinThreshold usage threshold) $ throwError $ MachineException OutOfGasError term

recordGasUsageAt :: Plain Term -> Gas -> CekM ()
recordGasUsageAt term gas = do
    cekStateGasUsage <>= gas
    checkGasAt term

-- | The monad the CEK machine runs in.
type CekM = ReaderT CekEnv (ExceptT CekMachineException (State CekState))

data Frame
    = FrameApplyFun VarEnv (Plain Value)               -- ^ @[V _]@
    | FrameApplyArg VarEnv (Plain Term)                -- ^ @[_ N]@
    | FrameTyInstArg (Type TyName ())                  -- ^ @{_ A}@
    | FrameUnwrap                                      -- ^ @(unwrap _)@
    | FrameIWrap () (Type TyName ()) (Type TyName ())  -- ^ @(iwrap A B _)@

type Context = [Frame]

evalCekM :: CekEnv -> CekM a -> Either CekMachineException a
evalCekM env = flip evalState emptyState . runExceptT . flip runReaderT env

runCekM :: CekEnv -> CekM a -> (CekState, Either CekMachineException a)
runCekM env = swap . flip runState emptyState . runExceptT . flip runReaderT env

-- | Get the current 'VarEnv'.
getVarEnv :: CekM VarEnv
getVarEnv = asks _cekEnvVarEnv

-- | Set a new 'VarEnv' and proceed.
withVarEnv :: VarEnv -> CekM a -> CekM a
withVarEnv = local . set cekEnvVarEnv

-- | Extend an environment with a variable name, the value the variable stands for
-- and the environment the value is defined in.
extendVarEnv :: Name () -> Plain Value -> VarEnv -> VarEnv -> VarEnv
extendVarEnv argName arg argVarEnv = insertByName argName $ Closure argVarEnv arg

-- | Look up a variable name in the environment.
lookupVarName :: Name () -> CekM Closure
lookupVarName varName = do
    varEnv <- getVarEnv
    case lookupName varName varEnv of
        Nothing   -> throwError $ MachineException OpenTermEvaluatedMachineError (Var () varName)
        Just clos -> pure clos

-- | Look up a 'DynamicBuiltinName' in the environment.
lookupDynamicBuiltinName :: DynamicBuiltinName -> CekM DynamicBuiltinNameMeaning
lookupDynamicBuiltinName dynName = do
    DynamicBuiltinNameMeanings means <- asks _cekEnvMeans
    case Map.lookup dynName means of
        Nothing   -> throwError $ MachineException err term where
            err  = OtherMachineError $ UnknownDynamicBuiltinNameErrorE dynName
            term = Builtin () $ DynBuiltinName () dynName
        Just mean -> pure mean

-- | The computing part of the CEK machine.
-- Either
-- 1. adds a frame to the context and calls 'computeCek' ('TyInst', 'Apply', 'IWrap', 'Unwrap')
-- 2. calls 'returnCek' on values ('TyAbs', 'LamAbs', 'Constant')
-- 3. returns 'EvaluationFailure' ('Error')
-- 4. looks up a variable in the environment and calls 'returnCek' ('Var')
computeCek :: Context -> Plain Term -> CekM EvaluationResultDef
computeCek con (TyInst _ body ty)       = computeCek (FrameTyInstArg ty : con) body
computeCek con (Apply _ fun arg)        = do
    varEnv <- getVarEnv
    computeCek (FrameApplyArg varEnv arg : con) fun
computeCek con (IWrap ann pat arg term) = computeCek (FrameIWrap ann pat arg : con) term
computeCek con (Unwrap _ term)          = computeCek (FrameUnwrap : con) term
computeCek con tyAbs@TyAbs{}            = returnCek con tyAbs
computeCek con lamAbs@LamAbs{}          = returnCek con lamAbs
computeCek con constant@Constant{}      = returnCek con constant
computeCek con bi@Builtin{}             = returnCek con bi
computeCek _   Error{}                  = pure EvaluationFailure
computeCek con (Var _ varName)          = do
    Closure newVarEnv term <- lookupVarName varName
    withVarEnv newVarEnv $ returnCek con term

-- | The returning part of the CEK machine.
-- Returns 'EvaluationSuccess' in case the context is empty, otherwise pops up one frame
-- from the context and either
-- 1. performs reduction and calls 'computeCek' ('FrameTyInstArg', 'FrameApplyFun', 'FrameUnwrap')
-- 2. performs a constant application and calls 'returnCek' ('FrameTyInstArg', 'FrameApplyFun')
-- 3. puts 'FrameApplyFun' on top of the context and proceeds with the argument from 'FrameApplyArg'
-- 4. grows the resulting term ('FrameWrap')
returnCek :: Context -> Plain Value -> CekM EvaluationResultDef
returnCek []                                  res = pure $ EvaluationSuccess res
returnCek (FrameTyInstArg ty           : con) fun = instantiateEvaluate con ty fun
returnCek (FrameApplyArg argVarEnv arg : con) fun = do
    funVarEnv <- getVarEnv
    withVarEnv argVarEnv $ computeCek (FrameApplyFun funVarEnv fun : con) arg
returnCek (FrameApplyFun funVarEnv fun : con) arg = do
    argVarEnv <- getVarEnv
    applyEvaluate funVarEnv argVarEnv con fun arg
returnCek (FrameIWrap ann pat arg      : con) val = returnCek con $ IWrap ann pat arg val
returnCek (FrameUnwrap                 : con) dat = case dat of
    IWrap _ _ _ term -> returnCek con term
    term             -> throwError $ MachineException NonWrapUnwrappedMachineError term

-- | Instantiate a term with a type and proceed.
-- In case of 'TyAbs' just ignore the type. Otherwise check if the term is an
-- iterated application of a 'BuiltinName' to a list of 'Value's and, if succesful,
-- apply the term to the type via 'TyInst'.
instantiateEvaluate :: Context -> Type TyName () -> Plain Term -> CekM EvaluationResultDef
instantiateEvaluate con ty tyabs@(TyAbs _ _ _ body) = do
    gas <- asks (view (cekEnvCostModel . costModelInstantiation))
    recordGasUsageAt (TyInst () tyabs ty) gas
    computeCek con body
instantiateEvaluate con ty fun
    -- This will be handled by the cost model for saturated builtins
    | isJust $ termAsPrimIterApp fun = returnCek con $ TyInst () fun ty
    | otherwise                      =
        throwError $ MachineException NonPrimitiveInstantiationMachineError fun

-- | Apply a function to an argument and proceed.
-- If the function is a 'LamAbs', then extend the current environment with a new variable and proceed.
-- If the function is not a 'LamAbs', then 'Apply' it to the argument and view this
-- as an iterated application of a 'BuiltinName' to a list of 'Value's.
-- If succesful, proceed with either this same term or with the result of the computation
-- depending on whether 'BuiltinName' is saturated or not.
applyEvaluate
    :: VarEnv -> VarEnv -> Context -> Plain Value -> Plain Value -> CekM EvaluationResultDef
applyEvaluate funVarEnv argVarEnv con fun@(LamAbs _ name _ body) arg = do
    -- we do this here since it's where we know it's not a builtin application,
    -- which we track separately
    gas <- asks (view (cekEnvCostModel . costModelApplication))
    recordGasUsageAt (Apply () fun arg) gas
    withVarEnv (extendVarEnv name arg argVarEnv funVarEnv) $ computeCek con body
applyEvaluate funVarEnv _         con fun                    arg =
    let term = Apply () fun arg in
        case termAsPrimIterApp term of
            Nothing                       ->
                throwError $ MachineException NonPrimitiveApplicationMachineError term
            Just (IterApp headName spine) -> do

                constAppResult <- applyStagedBuiltinName term headName spine
                withVarEnv funVarEnv $ case constAppResult of
                    ConstAppSuccess res -> computeCek con res
                    ConstAppFailure     -> pure EvaluationFailure
                    ConstAppStuck       -> returnCek con term
                    ConstAppError   err ->
                        throwError $ MachineException (ConstAppMachineError err) term

evaluateInCekM :: EvaluateConstApp (Either CekMachineException) a -> CekM (ConstAppResult a)
evaluateInCekM a = do
    cekEnv <- ask
    let eval means' = evalCekTermCatchIn $ cekEnv & cekEnvMeans <>~ means'
    liftEither $ runEvaluateConstApp eval a

-- | Apply a 'StagedBuiltinName' to a list of 'Value's.
applyStagedBuiltinName :: Plain Term -> StagedBuiltinName -> [Plain Value] -> CekM ConstAppResultDef
applyStagedBuiltinName _ (DynamicStagedBuiltinName name) args = do
    DynamicBuiltinNameMeaning sch x <- lookupDynamicBuiltinName name
    -- TODO: gas for dynamic builtins
    evaluateInCekM $ applyTypeSchemed sch x args
applyStagedBuiltinName term (StaticStagedBuiltinName name) args = do
    costFunction <- asks (view (cekEnvCostModel . costModelBuiltin))
    recordGasUsageAt term (costFunction name args)
    evaluateInCekM $ applyBuiltinName name args

-- | Evaluate a term in an environment using the CEK machine.
evalCekTermCatchIn
    :: CekEnv -> Plain Term -> Either CekMachineException EvaluationResultDef
evalCekTermCatchIn cekEnv = evalCekM cekEnv . computeCek []

runCekTermCatchIn
    :: CekEnv -> Plain Term -> (CekState, Either CekMachineException EvaluationResultDef)
runCekTermCatchIn cekEnv = runCekM cekEnv . computeCek []

-- | Evaluate a term using the CEK machine.
evalCekTermCatch
    :: GasThreshold
    -> DynamicBuiltinNameMeanings
    -> Plain Term
    -> Either CekMachineException EvaluationResultDef
evalCekTermCatch threshold means = evalCekTermCatchIn $ CekEnv means mempty defaultCostModel threshold

-- | Evaluate a term using the CEK machine.
runCekTermCatch
    :: GasThreshold
    -> DynamicBuiltinNameMeanings
    -> Plain Term
    -> (CekState, Either CekMachineException EvaluationResultDef)
runCekTermCatch threshold means = runCekTermCatchIn $ CekEnv means mempty defaultCostModel threshold

-- | Evaluate a term using the CEK machine. May throw a 'CekMachineException'.
evalCekTerm
    :: GasThreshold
    -> DynamicBuiltinNameMeanings
    -> Term TyName Name ()
    -> EvaluationResultDef
evalCekTerm threshold means term = either throw id $ evalCekTermCatch threshold means term

-- | Evaluate a term using the CEK machine. May throw a 'CekMachineException'.
runCekTerm
    :: GasThreshold
    -> DynamicBuiltinNameMeanings
    -> Term TyName Name ()
    -> (CekState, EvaluationResultDef)
runCekTerm threshold means term = either throw id <$> runCekTermCatch threshold means term

-- The implementation is a bit of a hack.
readKnownCek
    :: KnownType a
    => DynamicBuiltinNameMeanings
    -> Term TyName Name ()
    -> Either CekMachineException (EvaluationResult a)
readKnownCek means term = do
    res <- runReflectT $ readKnown (evalCekTermCatch Unbounded . mappend means) term
    case res of
        EvaluationFailure            -> Right EvaluationFailure
        EvaluationSuccess (Left err) -> Left $ MachineException appErr term where
            appErr = ConstAppMachineError $ UnreadableBuiltinConstAppError term err
        EvaluationSuccess (Right x)  -> Right $ EvaluationSuccess x

-- | Run a program using the CEK machine. May throw a 'CekMachineException'.
-- Calls 'evaluateCek' under the hood.
evalCekProgram
    :: GasThreshold
    -> DynamicBuiltinNameMeanings
    -> Program TyName Name ()
    -> EvaluationResultDef
evalCekProgram threshold means (Program _ _ term) = evalCekTerm threshold means term

-- | Run a program using the CEK machine. May throw a 'CekMachineException'.
-- Calls 'evaluateCek' under the hood.
runCekProgram
    :: GasThreshold
    -> DynamicBuiltinNameMeanings
    -> Program TyName Name ()
    -> (CekState, EvaluationResultDef)
runCekProgram threshold means (Program _ _ term) = runCekTerm threshold means term
