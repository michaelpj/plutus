{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module KeyBytes ( KeyBytes (..)
                , privKeyTrim
                , fromHex
                ) where

import           Codec.Serialise
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Lazy   as BSL
import           Data.Hashable          (Hashable)
import           Data.String            (IsString (..))
import           Data.Swagger.Internal
import           Data.Swagger.Schema
import           Data.Word              (Word8)
import           Language.PlutusTx.Lift
import           Web.HttpApiData        (FromHttpApiData (..), ToHttpApiData (..))

fromHex :: BSL.ByteString -> KeyBytes
fromHex = KeyBytes . asBSLiteral
    where

    handleChar :: Word8 -> Word8
    handleChar x
        | x >= 48 && x <= 57 = x - 48 -- hexits 0-9
        | x >= 97 && x <= 102 = x - 87 -- hexits a-f
        | x >= 65 && x <= 70 = x - 55 -- hexits A-F
        | otherwise = error "not a hexit"

    -- turns a pair of bytes such as "a6" into a single Word8
    handlePair :: Word8 -> Word8 -> Word8
    handlePair c c' = 16 * handleChar c + handleChar c'

    asBytes :: [Word8] -> [Word8]
    asBytes []        = mempty
    asBytes (c:c':cs) = handlePair c c' : asBytes cs
    asBytes _         = error "unpaired digit"

    -- parses a bytestring such as @a6b4@ into an actual bytestring
    asBSLiteral :: BSL.ByteString -> BSL.ByteString
    asBSLiteral = withBytes asBytes
        where withBytes f = BSL.pack . f . BSL.unpack

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
