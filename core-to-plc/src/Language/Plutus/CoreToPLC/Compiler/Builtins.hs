{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Functions for compiling Plutus Core builtins.
module Language.Plutus.CoreToPLC.Compiler.Builtins where

import qualified Language.Plutus.CoreToPLC.Builtins                  as Builtins
import           Language.Plutus.CoreToPLC.Compiler.Definitions
import {-# SOURCE #-} Language.Plutus.CoreToPLC.Compiler.Expr
import           Language.Plutus.CoreToPLC.Compiler.Names
import {-# SOURCE #-} Language.Plutus.CoreToPLC.Compiler.Type
import           Language.Plutus.CoreToPLC.Compiler.Types
import           Language.Plutus.CoreToPLC.Compiler.Utils
import           Language.Plutus.CoreToPLC.Compiler.ValueRestriction
import           Language.Plutus.CoreToPLC.Error
import           Language.Plutus.CoreToPLC.PLCTypes
import           Language.Plutus.CoreToPLC.Utils

import qualified Language.PlutusCore                                 as PLC
import qualified Language.PlutusCore.MkPlc                           as PLC
import           Language.PlutusCore.Quote
import qualified Language.PlutusCore.StdLib.Data.Bool                as Bool

import qualified GhcPlugins                                          as GHC

import qualified Language.Haskell.TH.Syntax                          as TH

import           Control.Monad
import           Control.Monad.Reader

import qualified Data.ByteString.Lazy                                as BSL
import qualified Data.Map                                            as Map

-- Plutus primitives

{- Note [Mapping builtins]
We want the user to be able to call the Plutus builtins as normal Haskell functions.

To do this, we provide a library of such functions, and then make a map from their TH names (which we can
derive from the real function declarations, to be sure they match up), to their corresponding GHC names
(in fact the TyThings, so we have the types too). Annoyingly, this has to be done in the GHC Core monad
and then passed to us.

We then define the builtin values for each of the GHC names, using the map to go between the two in the
cases where we do (mostly compiling known builtins for some typeclass operations).
-}

builtinNames :: [TH.Name]
builtinNames = [
    ''BSL.ByteString
    , ''Int
    , ''Bool
    , ''()

    , 'Builtins.concatenate
    , 'Builtins.takeByteString
    , 'Builtins.dropByteString
    , 'Builtins.sha2_256
    , 'Builtins.sha3_256
    , 'Builtins.equalsByteString

    , 'Builtins.verifySignature
    , 'Builtins.txhash
    , 'Builtins.blocknum

    , 'Builtins.addInteger
    , 'Builtins.subtractInteger
    , 'Builtins.multiplyInteger
    , 'Builtins.divideInteger
    , 'Builtins.remainderInteger
    , 'Builtins.greaterThanInteger
    , 'Builtins.greaterThanEqInteger
    , 'Builtins.lessThanInteger
    , 'Builtins.lessThanEqInteger
    , 'Builtins.equalsInteger

    , 'Builtins.error
    ]

getThing :: Converting m => TH.Name -> m GHC.TyThing
getThing name = do
    ConvertingContext{ccBuiltinNameInfo=names} <- ask
    case Map.lookup name names of
        Nothing    -> throwSd ConversionError $ "Missing builtin name:" GHC.<+> (GHC.text $ show name)
        Just thing -> pure thing

defineBuiltinTerm :: Converting m => TH.Name -> PLCTerm -> [GHC.Name] -> m ()
defineBuiltinTerm name term deps = do
    ghcId <- GHC.tyThingId <$> getThing name
    var <- convVarFresh ghcId
    defineTerm (GHC.getName ghcId) (Def Abstract var term) deps

defineBuiltinType :: Converting m => TH.Name -> Visibility -> PLCType -> [GHC.Name] -> m ()
defineBuiltinType name vis ty deps = do
    tc <- GHC.tyThingTyCon <$> getThing name
    var <- convTcTyVarFresh tc
    defineType (GHC.getName tc) (Def vis var (PlainType ty)) deps

defineBuiltinTerms :: Converting m => m ()
defineBuiltinTerms = do
    bs <- GHC.getName <$> getThing ''BSL.ByteString
    int <- GHC.getName <$> getThing ''Int
    bool <- GHC.getName <$> getThing ''Bool
    unit <- GHC.getName <$> getThing ''()

    -- Bytestring builtins
    do
        let term = instSize haskellBSSize $ mkConstant PLC.Concatenate
        defineBuiltinTerm 'Builtins.concatenate term [bs]
    do
        let term = instSize haskellBSSize $ instSize haskellIntSize $ mkConstant PLC.TakeByteString
        defineBuiltinTerm 'Builtins.takeByteString term [int, bs]
    do
        let term = instSize haskellBSSize $ instSize haskellIntSize $ mkConstant PLC.DropByteString
        defineBuiltinTerm 'Builtins.dropByteString term [int, bs]
    do
        let term = instSize haskellBSSize $ mkConstant PLC.SHA2
        defineBuiltinTerm 'Builtins.sha2_256 term [bs]
    do
        let term = instSize haskellBSSize $ mkConstant PLC.SHA3
        defineBuiltinTerm 'Builtins.sha3_256 term [bs]
    do
        term <- mkBsRel PLC.EqByteString
        defineBuiltinTerm 'Builtins.equalsByteString term [bs, bool]

    -- Integer builtins
    do
        let term = mkIntFun PLC.AddInteger
        defineBuiltinTerm 'Builtins.addInteger term [int]
    do
        let term = mkIntFun PLC.SubtractInteger
        defineBuiltinTerm 'Builtins.subtractInteger term [int]
    do
        let term = mkIntFun PLC.MultiplyInteger
        defineBuiltinTerm 'Builtins.multiplyInteger term [int]
    do
        let term = mkIntFun PLC.DivideInteger
        defineBuiltinTerm 'Builtins.divideInteger term [int]
    do
        let term = mkIntFun PLC.RemainderInteger
        defineBuiltinTerm 'Builtins.remainderInteger term [int]
    do
        term <- mkIntRel PLC.GreaterThanInteger
        defineBuiltinTerm 'Builtins.greaterThanInteger term [int, bool]
    do
        term <- mkIntRel PLC.GreaterThanEqInteger
        defineBuiltinTerm 'Builtins.greaterThanEqInteger term [int, bool]
    do
        term <- mkIntRel PLC.LessThanInteger
        defineBuiltinTerm 'Builtins.lessThanInteger term [int, bool]
    do
        term <- mkIntRel PLC.LessThanEqInteger
        defineBuiltinTerm 'Builtins.lessThanEqInteger term [int, bool]
    do
        term <- mkIntRel PLC.EqInteger
        defineBuiltinTerm 'Builtins.equalsInteger term [int, bool]

    -- Blockchain builtins
    do
        let term = mkConstant PLC.TxHash
        defineBuiltinTerm 'Builtins.txhash term [bs]
    do
        term <- wrapBsRel 3 $ instSize haskellBSSize $ instSize haskellBSSize $ instSize haskellBSSize $ mkConstant PLC.VerifySignature
        defineBuiltinTerm 'Builtins.verifySignature term [bs, bool]
    -- TODO: blocknum, this is annoying because we want to actually apply it to a size, which currently crashes in the evaluator
    -- as it's unimplemented

    -- Error
    do
        term <- errorFunc
        defineBuiltinTerm 'Builtins.error term [unit]

defineBuiltinTypes :: Converting m => m ()
defineBuiltinTypes = do
    do
        let ty = appSize haskellBSSize $ PLC.TyBuiltin () PLC.TyByteString
        defineBuiltinType ''BSL.ByteString Visible ty []
    do
        let ty = appSize haskellIntSize (PLC.TyBuiltin () PLC.TyInteger)
        defineBuiltinType ''Int Visible ty []

lookupBuiltinTerm :: Converting m => TH.Name -> m PLCTerm
lookupBuiltinTerm name = do
    ghcName <- GHC.getName <$> getThing name
    maybeTerm <- lookupTerm ghcName
    case maybeTerm of
        Just t  -> pure t
        Nothing -> throwPlain $ ConversionError "Missing builtin definition"

lookupBuiltinType :: Converting m => TH.Name -> m PLCType
lookupBuiltinType name = do
    ghcName <- GHC.getName <$> getThing name
    maybeType <- lookupType ghcName
    case maybeType of
        Just t  -> pure t
        Nothing -> throwPlain $ ConversionError "Missing builtin definition"

-- | The function 'error :: forall a . () -> a'.
errorFunc :: Converting m => m PLCTerm
errorFunc = do
    n <- liftQuote $ freshTyName () "e"
    -- see Note [Value restriction]
    mangleTyAbs $ PLC.TyAbs () n (PLC.Type ()) (PLC.Error () (PLC.TyVar () n))

-- | The type 'forall a. () -> a'.
errorTy :: Converting m => m PLCType
errorTy = do
    tyname <- safeFreshTyName "a"
    mangleTyForall $ PLC.TyForall () tyname (PLC.Type ()) (PLC.TyVar () tyname)

-- | Convert a Scott-encoded Boolean into a Haskell Boolean.
scottBoolToHaskellBool :: Converting m => m PLCTerm
scottBoolToHaskellBool = do
    scottBoolTy <- liftQuote Bool.getBuiltinBool
    haskellBoolTy <- convType GHC.boolTy

    arg <- liftQuote $ freshName () "b"
    let match = PLC.Var () arg
    let instantiatedMatch = PLC.TyInst () match haskellBoolTy

    haskellTrue <- convDataConRef GHC.trueDataCon
    haskellFalse <- convDataConRef GHC.falseDataCon
    pure $
        PLC.LamAbs () arg scottBoolTy $
        PLC.mkIterApp () instantiatedMatch [ haskellTrue, haskellFalse ]

-- | Convert a Haskell Boolean into a Scott-encoded Boolean.
haskellBoolToScottBool :: Converting m => m PLCTerm
haskellBoolToScottBool = do
    scottBoolTy <- liftQuote Bool.getBuiltinBool
    haskellBoolTy <- convType GHC.boolTy

    arg <- liftQuote $ freshName () "b"
    match <- getMatchInstantiated GHC.boolTy
    let instantiatedMatch = PLC.TyInst () match scottBoolTy

    scottTrue <- liftQuote Bool.getBuiltinTrue
    scottFalse <- liftQuote Bool.getBuiltinFalse
    pure $
        PLC.LamAbs () arg haskellBoolTy $
        PLC.mkIterApp () instantiatedMatch [ scottTrue, scottFalse ]

-- | Wrap an integer relation of arity @n@ that produces a Scott boolean.
wrapIntRel :: Converting m => Int -> PLCTerm -> m PLCTerm
wrapIntRel arity term = do
    intTy <- lookupBuiltinType ''Int
    args <- replicateM arity $ do
        name <- liftQuote $ freshName () "arg"
        pure $ PLC.VarDecl () name intTy

    converter <- scottBoolToHaskellBool

    pure $
        PLC.mkIterLamAbs () args $
        PLC.Apply () converter (PLC.mkIterApp () term (fmap (PLC.mkVar ()) args))

mkIntRel :: Converting m => PLC.BuiltinName -> m PLCTerm
mkIntRel name = wrapIntRel 2 $ instSize haskellIntSize (mkConstant name)

-- | Wrap an bytestring relation of arity @n@ that produces a Scott boolean.
wrapBsRel :: Converting m => Int -> PLCTerm -> m PLCTerm
wrapBsRel arity term = do
    bsTy <- lookupBuiltinType ''BSL.ByteString
    args <- replicateM arity $ do
        name <- liftQuote $ freshName () "arg"
        pure $ PLC.VarDecl () name bsTy

    converter <- scottBoolToHaskellBool

    pure $
        PLC.mkIterLamAbs () args $
        PLC.Apply () converter (PLC.mkIterApp () term (fmap (PLC.mkVar ()) args))

mkBsRel :: Converting m => PLC.BuiltinName -> m PLCTerm
mkBsRel name = wrapBsRel 2 $ instSize haskellBSSize (mkConstant name)
