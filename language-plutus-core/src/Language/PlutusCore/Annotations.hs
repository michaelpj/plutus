{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
module Language.PlutusCore.Annotations (HasAnnotation (..)) where

import           Data.Coerce
import           Language.PlutusCore.Name
import           Language.PlutusCore.Type
import           Lens.Micro

-- | The lens induced by back and forth coercions.
coerced :: Coercible a b => Lens' a b
coerced f x = coerce <$> f (coerce x)

class HasAnnotation a b where
    annotation :: Lens' a b

instance HasAnnotation (Name a) a where
    annotation = lens g s
        where
            g = nameAttribute
            s n a = n {nameAttribute=a}

instance HasAnnotation (TyName a) a where
    annotation = coerced @(TyName a) @(Name a) . annotation

instance HasAnnotation (Term tyname name a) a where
    annotation = lens g s
        where
            g = \case
                Var a _        -> a
                TyAbs a _ _ _  -> a
                Apply a _ _    -> a
                Constant a _   -> a
                TyInst a _ _   -> a
                Unwrap a _     -> a
                Wrap a _ _ _   -> a
                Error a _      -> a
                LamAbs a _ _ _ -> a
            s term a = case term of
                Var _ n         -> Var a n
                TyAbs _ tn k t  -> TyAbs a tn k t
                Apply _ t1 t2   -> Apply a t1 t2
                Constant _ c    -> Constant a c
                TyInst _ t ty   -> TyInst a t ty
                Unwrap _ t      -> Unwrap a t
                Wrap _ tn ty t  -> Wrap a tn ty t
                Error _ ty      -> Error a ty
                LamAbs _ n ty t -> LamAbs a n ty t

instance HasAnnotation (Type tyname a) a where
    annotation = lens g s
        where
            g = \case
                TyVar a _        -> a
                TyFun a _ _      -> a
                TyFix a _ _      -> a
                TyForall a _ _ _ -> a
                TyBuiltin a _    -> a
                TyInt a _        -> a
                TyLam a _ _ _    -> a
                TyApp a _ _      -> a
            s typ a = case typ of
                TyVar _ n         -> TyVar a n
                TyFun _ i o       -> TyFun a i o
                TyFix _ n ty      -> TyFix a n ty
                TyForall _ n k ty -> TyForall a n k ty
                TyBuiltin _ b     -> TyBuiltin a b
                TyInt _ i         -> TyInt a i
                TyLam _ tn k ty   -> TyLam a tn k ty
                TyApp _ t1 t2     -> TyApp a t1 t2

instance HasAnnotation (Constant a) a where
    annotation = lens g s
        where
            g = \case
                BuiltinInt a _ _   -> a
                BuiltinBS a _ _    -> a
                BuiltinSize a _    -> a
                BuiltinName a _    -> a
                DynBuiltinName a _ -> a
            s con a = case con of
                BuiltinInt _ sz i  -> BuiltinInt a sz i
                BuiltinBS _ sz bs  -> BuiltinBS a sz bs
                BuiltinSize _ sz   -> BuiltinSize a sz
                BuiltinName _ n    -> BuiltinName a n
                DynBuiltinName _ n -> DynBuiltinName a n
