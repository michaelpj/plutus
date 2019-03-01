{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module KeyBytes ( KeyBytes (..)
                , privKeyTrim
                ) where

import           Codec.Serialise
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Lazy   as BSL
import           Data.Hashable          (Hashable)
import           Data.String            (IsString (..))
import           Data.Swagger.Internal
import           Data.Swagger.Schema
import           Language.PlutusTx.Lift
import           Web.HttpApiData        (FromHttpApiData (..), ToHttpApiData (..))

newtype KeyBytes = KeyBytes { getKeyBytes :: BSL.ByteString } -- TODO: use strict bytestring
    deriving (Eq, Ord, Show, IsString, Hashable, Serialise)

-- convert a private key to a public key
-- TODO: verify that this doesn't have sidechannels; maybe use ScrubbedBytes ??
privKeyTrim :: KeyBytes -> KeyBytes
privKeyTrim (KeyBytes bs) = KeyBytes (BSL.drop 32 bs)

makeLift ''KeyBytes

instance ToSchema KeyBytes where
    declareNamedSchema _ = pure $ NamedSchema (Just "KeyBytes") byteSchema

instance ToJSON KeyBytes where
    toJSON = undefined

instance FromJSON KeyBytes where
    parseJSON = undefined

instance ToHttpApiData KeyBytes where
    toUrlPiece = undefined

instance FromHttpApiData KeyBytes where
    parseUrlPiece = undefined
