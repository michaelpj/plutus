{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.PlutusCore.Type ( Term (..)
                                , Value
                                , Type (..)
                                , Kind (..)
                                , Program (..)
                                , Constant (..)
                                , Builtin (..)
                                , BuiltinName (..)
                                , DynamicBuiltinName (..)
                                , StagedBuiltinName (..)
                                , TypeBuiltin (..)
                                , Size
                                , Gas (..)
                                -- * Base functors
                                , TermF (..)
                                , TypeF (..)
                                , KindF (..)
                                -- * Helper functions
                                , alphaEqKind
                                , alphaEqTy
                                , alphaEqTerm
                                , alphaEqProg
                                , tyLoc
                                , termLoc
                                -- * Normalized
                                , Normalized (..)
                                -- * Backwards compatibility
                                , NormalizedType
                                , pattern NormalizedType
                                , getNormalizedType
                                ) where

import           Control.Lens
import qualified Data.ByteString.Lazy           as BSL
import           Data.Functor.Foldable
import qualified Data.Map                       as M
import           Instances.TH.Lift              ()
import           Language.Haskell.TH.Syntax     (Lift)
import           Language.PlutusCore.Lexer.Type
import           PlutusPrelude

type Size = Natural

newtype Gas = Gas
    { unGas :: Natural
    }

-- | A 'Type' assigned to expressions.
data Type tyname a = TyVar a (tyname a)
                   | TyFun a (Type tyname a) (Type tyname a)
                   | TyIFix a (Type tyname a) (Type tyname a)
                     -- ^ Fix-point type, for constructing self-recursive types
                   | TyForall a (tyname a) (Kind a) (Type tyname a)
                   | TyBuiltin a TypeBuiltin -- ^ Builtin type
                   | TyInt a Natural -- ^ Type-level size
                   | TyLam a (tyname a) (Kind a) (Type tyname a)
                   | TyApp a (Type tyname a) (Type tyname a)
                   deriving (Functor, Show, Generic, NFData, Lift, Eq, Ord)

data TypeF tyname a x = TyVarF a (tyname a)
                      | TyFunF a x x
                      | TyIFixF a x x
                      | TyForallF a (tyname a) (Kind a) x
                      | TyBuiltinF a TypeBuiltin
                      | TyIntF a Natural
                      | TyLamF a (tyname a) (Kind a) x
                      | TyAppF a x x
                      deriving (Functor, Traversable, Foldable)

type instance Base (Type tyname a) = TypeF tyname a

instance Recursive (Type tyname a) where
    project (TyVar l tn)         = TyVarF l tn
    project (TyFun l ty ty')     = TyFunF l ty ty'
    project (TyIFix l pat arg)   = TyIFixF l pat arg
    project (TyForall l tn k ty) = TyForallF l tn k ty
    project (TyBuiltin l b)      = TyBuiltinF l b
    project (TyInt l n)          = TyIntF l n
    project (TyLam l tn k ty)    = TyLamF l tn k ty
    project (TyApp l ty ty')     = TyAppF l ty ty'

instance Corecursive (Type tyname a) where
    embed (TyVarF l tn)         = TyVar l tn
    embed (TyFunF l ty ty')     = TyFun l ty ty'
    embed (TyIFixF l pat arg)   = TyIFix l pat arg
    embed (TyForallF l tn k ty) = TyForall l tn k ty
    embed (TyBuiltinF l b)      = TyBuiltin l b
    embed (TyIntF l n)          = TyInt l n
    embed (TyLamF l tn k ty)    = TyLam l tn k ty
    embed (TyAppF l ty ty')     = TyApp l ty ty'

-- this type is used for replacing type names in
-- the Eq instance
type EqTyState tyname a = M.Map (tyname a) (tyname a)

rebindAndEqTy :: (Eq a, Ord (tyname a))
              => EqTyState tyname a
              -> Type tyname a
              -> Type tyname a
              -> tyname a
              -> tyname a
              -> Bool
rebindAndEqTy eqSt tyLeft tyRight tnLeft tnRight =
    let intermediateSt = M.insert tnRight tnLeft eqSt
    in alphaEqTypeSt intermediateSt tyLeft tyRight

-- This tests for equality of names inside a monad that allows substitution.
alphaEqTypeSt :: (Ord (tyname a), Eq a)
        => EqTyState tyname a
        -> Type tyname a
        -> Type tyname a
        -> Bool
alphaEqTypeSt eqSt l r = case (l, r) of
    (TyFun _ domLeft codLeft, TyFun _ domRight codRight) ->
        alphaEqTypeSt eqSt domLeft domRight && alphaEqTypeSt eqSt codLeft codRight
    (TyApp _ fLeft aLeft, TyApp _ fRight aRight) ->
        alphaEqTypeSt eqSt fLeft fRight && alphaEqTypeSt eqSt aLeft aRight
    (TyInt _ nLeft, TyInt _ nRight) ->
        nLeft == nRight
    (TyBuiltin _ bLeft, TyBuiltin _ bRight) ->
        bLeft == bRight
    (TyIFix _ patLeft argLeft, TyIFix _ patRight argRight) ->
        alphaEqTypeSt eqSt patLeft patRight && alphaEqTypeSt eqSt argLeft argRight
    (TyForall _ tnLeft kLeft tyLeft ,TyForall _ tnRight kRight tyRight) ->
        let tyEq = rebindAndEqTy eqSt tyLeft tyRight tnLeft tnRight
        in (kLeft `alphaEqKind` kRight && tyEq)
    (TyLam _ tnLeft kLeft tyLeft, TyLam _ tnRight kRight tyRight) ->
        let tyEq = rebindAndEqTy eqSt tyLeft tyRight tnLeft tnRight
        in (kLeft `alphaEqKind` kRight && tyEq)
    (TyVar _ tnRight, TyVar _ tnLeft) ->
        case M.lookup tnLeft eqSt of
            Just tn -> tnRight == tn
            Nothing -> tnRight == tnLeft
    (_, _) -> False

alphaEqTy :: (Ord (tyname a), Eq a)
        => Type tyname a
        -> Type tyname a
        -> Bool
alphaEqTy = alphaEqTypeSt mempty

data EqState tyname name a = EqState { _tyMap :: M.Map (tyname a) (tyname a), _termMap :: M.Map (name a) (name a) }

emptyEqState :: (Ord (tyname a), Ord (name a)) => EqState tyname name a
emptyEqState = EqState mempty mempty

termMap :: Lens' (EqState tyname name a) (M.Map (name a) (name a))
termMap f s = fmap (\x -> s { _termMap = x }) (f (_termMap s))

tyMap :: Lens' (EqState tyname name a) (M.Map (tyname a) (tyname a))
tyMap f s = fmap (\x -> s { _tyMap = x }) (f (_tyMap s))

rebindAndEq :: (Eq a, Ord (name a), Ord (tyname a))
            => EqState tyname name a
            -> Term tyname name a
            -> Term tyname name a
            -> name a
            -> name a
            -> Bool
rebindAndEq eqSt tLeft tRight nLeft nRight =
    let intermediateSt = over termMap (M.insert nRight nLeft) eqSt
    in alphaEqTermSt intermediateSt tLeft tRight

alphaEqTermSt :: (Ord (name a), Ord (tyname a), Eq a)
         => EqState tyname name a
         -> Term tyname name a
         -> Term tyname name a
         -> Bool
alphaEqTermSt eqSt l r = case (l, r) of
    (TyAbs _ tnLeft kLeft tLeft, TyAbs _ tnRight kRight tRight) ->
        let intermediateSt = over tyMap (M.insert tnRight tnLeft) eqSt
        in kLeft `alphaEqKind` kRight && alphaEqTermSt intermediateSt tLeft tRight
    (IWrap _ patLeft argLeft termLeft, IWrap _ patRight argRight termRight) ->
        alphaEqTypeSt (_tyMap eqSt) patLeft patRight &&
        alphaEqTypeSt (_tyMap eqSt) argLeft argRight &&
        alphaEqTermSt eqSt termLeft termRight
    (LamAbs _ nLeft tyLeft tLeft, LamAbs _ nRight tyRight tRight) ->
        let tEq = rebindAndEq eqSt tLeft tRight nLeft nRight
        in alphaEqTypeSt (_tyMap eqSt) tyLeft tyRight && tEq
    (Apply _ fLeft aLeft, Apply _ fRight aRight) ->
        alphaEqTermSt eqSt fLeft fRight && alphaEqTermSt eqSt aLeft aRight
    (Constant _ cLeft, Constant _ cRight) ->
        cLeft == cRight
    (Builtin _ biLeft, Builtin _ biRight) ->
        biLeft == biRight
    (TyInst _ tLeft tyLeft, TyInst _ tRight tyRight) ->
        alphaEqTermSt eqSt tLeft tRight && alphaEqTypeSt (_tyMap eqSt) tyLeft tyRight
    (Error _ tyLeft, Error _ tyRight) ->
        alphaEqTypeSt (_tyMap eqSt) tyLeft tyRight
    (Var _ nRight, Var _ nLeft) ->
        case M.lookup nLeft (_termMap eqSt) of
            Just n  -> nRight == n
            Nothing -> nRight == nLeft
    (_, _) -> False

alphaEqTerm :: (Ord (name a), Ord (tyname a), Eq a)
         => Term tyname name a
         -> Term tyname name a
         -> Bool
alphaEqTerm = alphaEqTermSt emptyEqState

tyLoc :: Type tyname a -> a
tyLoc (TyVar l _)        = l
tyLoc (TyFun l _ _)      = l
tyLoc (TyIFix l _ _)     = l
tyLoc (TyForall l _ _ _) = l
tyLoc (TyBuiltin l _)    = l
tyLoc (TyInt l _)        = l
tyLoc (TyLam l _ _ _)    = l
tyLoc (TyApp l _ _)      = l

termLoc :: Term tyname name a -> a
termLoc (Var l _)        = l
termLoc (TyAbs l _ _ _)  = l
termLoc (Apply l _ _)    = l
termLoc (Constant l _)   = l
termLoc (Builtin l _)    = l
termLoc (TyInst l _ _)   = l
termLoc (Unwrap l _)     = l
termLoc (IWrap l _ _ _)  = l
termLoc (Error l _ )     = l
termLoc (LamAbs l _ _ _) = l

data Builtin a = BuiltinName a BuiltinName
               | DynBuiltinName a DynamicBuiltinName
               deriving (Functor, Show, Eq, Ord, Generic, NFData, Lift)

-- | A constant value.
data Constant a = BuiltinInt a Natural Integer
                | BuiltinBS a Natural BSL.ByteString
                | BuiltinSize a Natural
                | BuiltinStr a String
                deriving (Functor, Show, Eq, Ord, Generic, NFData, Lift)

-- TODO make this parametric in tyname as well
-- | A 'Term' is a value.
data Term tyname name a = Var a (name a) -- ^ A named variable
                        | TyAbs a (tyname a) (Kind a) (Term tyname name a)
                        | LamAbs a (name a) (Type tyname a) (Term tyname name a)
                        | Apply a (Term tyname name a) (Term tyname name a)
                        | Constant a (Constant a) -- ^ A constant term
                        | Builtin a (Builtin a)
                        | TyInst a (Term tyname name a) (Type tyname a)
                        | Unwrap a (Term tyname name a)
                        | IWrap a (Type tyname a) (Type tyname a) (Term tyname name a)
                        | Error a (Type tyname a)
                        deriving (Functor, Show, Generic, NFData, Lift, Eq, Ord)

data TermF tyname name a x = VarF a (name a)
                           | TyAbsF a (tyname a) (Kind a) x
                           | LamAbsF a (name a) (Type tyname a) x
                           | ApplyF a x x
                           | ConstantF a (Constant a)
                           | BuiltinF a (Builtin a)
                           | TyInstF a x (Type tyname a)
                           | UnwrapF a x
                           | IWrapF a (Type tyname a) (Type tyname a) x
                           | ErrorF a (Type tyname a)
                           deriving (Functor, Traversable, Foldable)

type instance Base (Term tyname name a) = TermF tyname name a

type Value = Term

instance Recursive (Term tyname name a) where
    project (Var x n)           = VarF x n
    project (TyAbs x n k t)     = TyAbsF x n k t
    project (LamAbs x n ty t)   = LamAbsF x n ty t
    project (Apply x t t')      = ApplyF x t t'
    project (Constant x c)      = ConstantF x c
    project (Builtin x bi)      = BuiltinF x bi
    project (TyInst x t ty)     = TyInstF x t ty
    project (Unwrap x t)        = UnwrapF x t
    project (IWrap x pat arg t) = IWrapF x pat arg t
    project (Error x ty)        = ErrorF x ty

instance Corecursive (Term tyname name a) where
    embed (VarF x n)           = Var x n
    embed (TyAbsF x n k t)     = TyAbs x n k t
    embed (LamAbsF x n ty t)   = LamAbs x n ty t
    embed (ApplyF x t t')      = Apply x t t'
    embed (ConstantF x c)      = Constant x c
    embed (BuiltinF x bi)      = Builtin x bi
    embed (TyInstF x t ty)     = TyInst x t ty
    embed (UnwrapF x t)        = Unwrap x t
    embed (IWrapF x pat arg t) = IWrap x pat arg t
    embed (ErrorF x ty)        = Error x ty

-- | Kinds. Each type has an associated kind.
data Kind a = Type a
            | KindArrow a (Kind a) (Kind a)
            | Size a
            deriving (Functor, Eq, Ord, Show, Generic, NFData, Lift)

data KindF a x = TypeF a
               | KindArrowF a x x
               | SizeF a
               deriving (Functor)

type instance Base (Kind a) = KindF a

instance Recursive (Kind a) where
    project (Type l)           = TypeF l
    project (KindArrow l k k') = KindArrowF l k k'
    project (Size l)           = SizeF l

alphaEqKind :: Eq a
        => Kind a
        -> Kind a
        -> Bool
alphaEqKind k1 k2 = k1 == k2

-- | A 'Program' is simply a 'Term' coupled with a 'Version' of the core
-- language.
data Program tyname name a = Program a (Version a) (Term tyname name a)
                 deriving (Show, Eq, Ord, Functor, Generic, NFData, Lift)

alphaEqProg :: (Ord (name a), Ord (tyname a), Eq a)
         => Program tyname name a
         -> Program tyname name a
         -> Bool
alphaEqProg (Program _ v1 t1) (Program _ v2 t2) = v1 == v2 && alphaEqTerm t1 t2

newtype Normalized a = Normalized { getNormalized :: a }
    deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
    deriving newtype NFData

instance Applicative Normalized where
    pure = Normalized
    Normalized f <*> Normalized x = Normalized $ f x

type NormalizedType tyname a = Normalized (Type tyname a)

{-# COMPLETE NormalizedType #-}
pattern NormalizedType :: Type tyname a -> NormalizedType tyname a
pattern NormalizedType ty = Normalized ty

getNormalizedType :: NormalizedType tyname a -> Type tyname a
getNormalizedType (Normalized ty) = ty

instance PrettyBy config a => PrettyBy config (Normalized a) where
    prettyBy config (Normalized x) = prettyBy config x
