{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}

module Language.PlutusCore.Error ( Error (..)
                                 ) where

import           Language.PlutusCore.Is
import           Language.PlutusCore.Lexer
import           Language.PlutusCore.Normalize
import           Language.PlutusCore.PrettyCfg
import           Language.PlutusCore.Renamer
import           Language.PlutusCore.TypeSynthesis
import           PlutusPrelude

import           Data.Text                         as T

data Error a = ParseError (ParseError a)
             | RenameError (RenameError a)
             | TypeError (TypeError a)
             | NormalizationError (NormalizationError a)
             | OtherError T.Text
             deriving (Generic, NFData)

{-
class IsError f where

    asError :: f a -> Error a

    asLeft :: f a -> Either (Error a) b
    asLeft = Left . asError

    convertError :: Either (f a) b -> Either (Error a) b
    convertError = first asError

    collectErrors :: (IsError g) => Either (f a) (Either (g a) b) -> Either (Error a) b
    collectErrors (Left x)          = asLeft x
    collectErrors (Right (Left x))  = asLeft x
    collectErrors (Right (Right x)) = Right x
-}

instance Is (ParseError a) (Error a) where
    embed = ParseError

instance Is (RenameError a) (Error a) where
    embed = RenameError

instance Is (TypeError a) (Error a) where
    embed = TypeError

instance Is (NormalizationError a) (Error a) where
    embed = NormalizationError

instance (PrettyCfg a) => PrettyCfg (Error a) where
    prettyCfg cfg (ParseError e)         = prettyCfg cfg e
    prettyCfg cfg (RenameError e)        = prettyCfg cfg e
    prettyCfg cfg (TypeError e)          = prettyCfg cfg e
    prettyCfg cfg (NormalizationError e) = prettyCfg cfg e
    prettyCfg _ (OtherError e)           = pretty e
