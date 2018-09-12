{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

-- | This module makes sure terms and types are well-formed according to Fig. 2
module Language.PlutusCore.Normalize ( check
                                     , checkProgram
                                     , checkTerm
                                     , NormalizationError
                                     , isTypeValue
                                     ) where

import           Control.Monad.Except

import           Data.Functor.Foldable
import           Data.Functor.Foldable.Monadic

import           Language.PlutusCore.Error
import           Language.PlutusCore.Name
import           Language.PlutusCore.Type
import           PlutusPrelude

-- | Ensure that all terms and types are well-formed accoring to Fig. 2
checkProgram :: (MonadError (Error a) m) => Program Type TyName Name a -> m (Program NormalizedType TyName Name a)
checkProgram p = liftEither $ convertError $ check p

-- | Ensure that all terms and types are well-formed accoring to Fig. 2
checkTerm :: (MonadError (Error a) m) => Term Type TyName Name a -> m (Term NormalizedType TyName Name a)
checkTerm p = liftEither $ convertError $ checkTerm p

-- | Ensure that all terms and types are well-formed accoring to Fig. 2
check :: Program Type tyname name a -> Either (NormalizationError tyname name a) (Program NormalizedType tyname name a)
check (Program l v t) = Program l v <$> checkT t

-- this basically ensures all type instatiations, etc. occur only with type *values*
checkT :: Term Type tyname name a -> Either (NormalizationError tyname name a) (Term NormalizedType tyname name a)
checkT (Error l ty)      = Error l <$> typeValue ty
checkT (TyInst l t ty)   = TyInst l <$> checkT t <*> typeValue ty
checkT (Wrap l tn ty t)  = Wrap l tn <$> typeValue ty <*> checkT t
checkT (Unwrap l t)      = Unwrap l <$> checkT t
checkT (LamAbs l n ty t) = LamAbs l n <$> typeValue ty <*> checkT t
checkT (Apply l t t')    = Apply l <$> checkT t <*> checkT t'
checkT (TyAbs l tn k t)  = TyAbs l tn k <$> termValue t
checkT (Var l n)         = pure $ Var l n
checkT (Constant l c)    = pure $ Constant l c

-- ensure a term is a value
termValue :: Term Type tyname name a -> Either (NormalizationError tyname name a) (Term NormalizedType tyname name a)
termValue (LamAbs l n ty t) = LamAbs l n <$> typeValue ty <*> checkT t
termValue (Wrap l tn ty t)  = Wrap l tn <$> typeValue ty <*> termValue t
termValue (TyAbs l tn k t)  = TyAbs l tn k <$> termValue t
termValue t                 = builtinValue t

builtinValue :: Term Type tyname name a -> Either (NormalizationError tyname name a) (Term NormalizedType tyname name a)
builtinValue (Constant l c)  = pure $ Constant l c
builtinValue (TyInst l t ty) = TyInst l <$> builtinValue t <*> (pure $ NormalizedType ty) -- TODO: this can contain redexes?
builtinValue (Apply l t t')  = Apply l <$> builtinValue t <*> termValue t'
builtinValue t               = Left $ BadTerm (termLoc t) t "builtin value"

isTypeValue :: Type tyname a -> Bool
isTypeValue = isRight . typeValue

-- ensure that a type is a type value
typeValue :: Type tyname a -> Either (NormalizationError tyname name a) (NormalizedType tyname a)
typeValue = fmap NormalizedType . cataM aM where

    aM ty | isTyValue ty = pure (embed ty)
          | otherwise    = neutralType (embed ty)

    isTyValue TyFunF{}     = True
    isTyValue TyForallF{}  = True
    isTyValue TyFixF{}     = True
    isTyValue TyLamF{}     = True
    isTyValue TyBuiltinF{} = True
    isTyValue TyIntF{}     = True
    isTyValue _            = False

-- ensure a type is a neutral type
neutralType :: Type tyname a -> Either (NormalizationError tyname name a) (Type tyname a)
neutralType = cataM aM where

    aM ty | isNeutralType ty = pure (embed ty)
          | otherwise        = Left (BadType (tyLocF ty) (embed ty) "neutral type")

    isNeutralType TyVarF{} = True
    isNeutralType TyAppF{} = True
    isNeutralType _        = False

    tyLocF = tyLoc . embed
