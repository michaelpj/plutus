{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Plutus.CoreToPLC.Utils where

import qualified Language.PlutusCore  as PLC

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import           GHC.Natural

strToBs :: String -> BSL.ByteString
strToBs = BSL.fromStrict . TE.encodeUtf8 . T.pack

bsToStr :: BSL.ByteString -> String
bsToStr = T.unpack . TE.decodeUtf8 . BSL.toStrict

instSize :: Natural -> PLC.Term tyname name () -> PLC.Term tyname name ()
instSize n t = PLC.TyInst () t (PLC.TyInt () n)

appSize :: Natural -> PLC.Type tyname () -> PLC.Type tyname ()
appSize n t = PLC.TyApp () t (PLC.TyInt () n)

mkConstant :: PLC.BuiltinName -> PLC.Term tyname name ()
mkConstant n = PLC.Constant () $ PLC.BuiltinName () n

mkIntFun :: PLC.BuiltinName -> PLC.Term PLC.TyName PLC.Name ()
mkIntFun name = instSize haskellIntSize (mkConstant name)

haskellIntSize :: Natural
haskellIntSize = 64

-- This is mostly so they are compatible with the output of the SHA functions
haskellBSSize :: Natural
haskellBSSize = 256
