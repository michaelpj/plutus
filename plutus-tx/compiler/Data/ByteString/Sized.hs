{-# LANGUAGE DeriveGeneric #-}
-- Types for 'ByteString's of length 32 and 64.
module Data.ByteString.Sized(
      ByteString32(..)
    , byteString32
    , ByteString64(..)
    , byteString64
    -- Operations
    , take32
    , append32
    , drop32
    ) where

import           Codec.Serialise      (Serialise)
import qualified Data.ByteString.Lazy as BSL
import           Data.String          (IsString (..))
import           GHC.Generics         (Generic)
import           GHC.Int              (Int64)

-- | A 'BSL.ByteString' of at most 32 bytes.
newtype ByteString32 = ByteString32 { unByteString32 :: BSL.ByteString }
        deriving (Eq, Ord, Show, Generic, IsString, Serialise)

-- | Turn a 'BSL.ByteString' into a 'ByteString32' by taking the first 32 bytes.
byteString32 :: BSL.ByteString -> ByteString32
byteString32 = ByteString32 . BSL.take 32

-- | A 'BSL.ByteString' of at most 64 bytes.
newtype ByteString64 = ByteString64 { unByteString64 :: BSL.ByteString }
        deriving (Eq, Ord, Show, IsString, Serialise)

-- TODO: JSON/Swagger instances here or in wallet-api??

-- | Turn a 'BSL.ByteString' into a 'ByteString32' by taking the first 32 bytes.
byteString64 :: BSL.ByteString -> ByteString64
byteString64 = ByteString64 . BSL.take 64

take32 :: Int64 -> ByteString32 -> ByteString32
take32 i (ByteString32 bs) = ByteString32 (BSL.take i bs)

append32 :: ByteString32 -> ByteString32 -> ByteString32
append32 (ByteString32 l) (ByteString32 r) = ByteString32 (BSL.append l r)

drop32 :: Int64 ->  ByteString32 -> ByteString32
drop32 i (ByteString32 bs) = ByteString32 (BSL.drop i bs)
