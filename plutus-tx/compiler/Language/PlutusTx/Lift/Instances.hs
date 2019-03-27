{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.PlutusTx.Lift.Instances () where

import qualified Language.PlutusCore          as PLC

import           Language.PlutusTx.Lift.Class
import           Language.PlutusTx.Utils

import           Language.PlutusIR

import qualified Data.ByteString.Sized        as BSS
import           Data.Proxy

-- Derived instances

-- This instance ensures that we can apply typeable type constructors to typeable arguments and get a typeable
-- type. We need the kind variable, so that partial application of type constructors works.
instance (Typeable (f :: * -> k), Typeable (a :: *)) => Typeable (f a) where
    typeRep _ = TyApp () <$> typeRep (undefined :: Proxy f) <*> typeRep (undefined :: Proxy a)

instance (Typeable (a :: *), Typeable (b :: *)) => Typeable (a -> b) where
    typeRep _ = TyFun () <$> typeRep (undefined :: Proxy a) <*> typeRep (undefined :: Proxy b)

-- Primitives

instance Typeable Int where
    typeRep _ = pure $ appSize haskellIntSize (TyBuiltin () PLC.TyInteger)

instance Lift Int where
    lift i = pure $ Constant () $ PLC.BuiltinInt () haskellIntSize $ fromIntegral i

instance Typeable BSS.ByteString32 where
    typeRep _ = pure $ appSize haskellBS32Size (TyBuiltin () PLC.TyByteString)

instance Lift BSS.ByteString32 where
    lift (BSS.ByteString32 bs) = pure $ Constant () $ PLC.BuiltinBS () haskellBS32Size bs

instance Typeable BSS.ByteString64 where
    typeRep _ = pure $ appSize haskellBS64Size (TyBuiltin () PLC.TyByteString)

instance Lift BSS.ByteString64 where
    lift (BSS.ByteString64 bs) = pure $ Constant () $ PLC.BuiltinBS () haskellBS64Size bs

-- Standard types
-- These need to be in a separate file for TH staging reasons

makeLift ''Bool
makeLift ''Maybe
makeLift ''Either
makeLift ''[]
-- include a few tuple instances for convenience
makeLift ''(,)
makeLift ''(,,)
makeLift ''(,,,)
makeLift ''(,,,,)
