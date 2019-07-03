{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Language.PlutusCore.Constant.Dynamic.Sealed (
    Sealed (..),
    dynamicSealName,
    dynamicSealDefinition,
    dynamicUnsealName,
    dynamicUnsealDefinition) where

import           Control.Monad.Except
import           Data.Proxy
import           Data.Text.Prettyprint.Doc
import           Language.PlutusCore.Constant.Typed
import           Language.PlutusCore.Constant.Make
import           Language.PlutusCore.Constant.Dynamic.Instances  ()
import           Language.PlutusCore.Lexer.Type
import           Language.PlutusCore.Type

newtype Sealed a = Sealed { unSealed :: a }
    deriving stock (Show, Eq, Ord)

instance Pretty a => Pretty (Sealed a) where
    pretty (Sealed a) = "sealed" <+> pretty a

dynamicSealName :: DynamicBuiltinName
dynamicSealName = DynamicBuiltinName "seal"

dynamicSealMeaning :: DynamicBuiltinNameMeaning
dynamicSealMeaning = DynamicBuiltinNameMeaning sch Sealed where
    sch =
        TypeSchemeAllType @"a" @0 Proxy $ \(_ :: Proxy a) ->
            Proxy @a `TypeSchemeArrow` TypeSchemeResult (Proxy @(Sealed a))

dynamicSealDefinition :: DynamicBuiltinNameDefinition
dynamicSealDefinition = DynamicBuiltinNameDefinition dynamicSealName dynamicSealMeaning

dynamicUnsealName :: DynamicBuiltinName
dynamicUnsealName = DynamicBuiltinName "unseal"

dynamicUnsealMeaning :: DynamicBuiltinNameMeaning
dynamicUnsealMeaning = DynamicBuiltinNameMeaning sch unSealed where
    sch =
        TypeSchemeAllType @"a" @0 Proxy $ \(_ :: Proxy a) ->
            Proxy @(Sealed a) `TypeSchemeArrow` TypeSchemeResult (Proxy @a)

dynamicUnsealDefinition :: DynamicBuiltinNameDefinition
dynamicUnsealDefinition = DynamicBuiltinNameDefinition dynamicUnsealName dynamicUnsealMeaning

instance (Pretty a, KnownType a) => KnownType (Sealed a) where
    toTypeAst _ = TyApp () (TyBuiltin () TySealed) (toTypeAst (Proxy @a))

    makeKnown (Sealed inner) = Apply ()
        (TyInst () (dynamicBuiltinNameAsTerm dynamicSealName) (toTypeAst (Proxy @a)))
        (makeKnown inner)

    readKnown eval term = do
        res <- makeRightReflectT $ eval mempty term
        case res of
            Apply () (Builtin () (DynBuiltinName () n)) a | n == dynamicSealName -> Sealed <$> readKnown eval a
            _                            -> throwError "Not an application of seal"
