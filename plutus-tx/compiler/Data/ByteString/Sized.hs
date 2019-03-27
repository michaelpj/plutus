module Data.ByteString.Sized(
      ByteString32(..)
    , ByteString64(..)
    -- Operations
    , take32
    , append32
    , drop32
    ) where


import qualified Data.ByteString.Lazy as BSL
import           Data.String          (IsString (..))
import           GHC.Int (Int64)

-- | A 'BSL.ByteString' of 32 bytes.
newtype ByteString32 = ByteString32 { unByteString32 :: BSL.ByteString }
        deriving (Eq, Ord, Show, IsString)

-- | A 'BSL.ByteString' of 64 bytes.
newtype ByteString64 = ByteString64 { unByteString64 :: BSL.ByteString }
        deriving (Eq, Ord, Show, IsString)

take32 :: Int64 -> ByteString32 -> ByteString32
take32 i (ByteString32 bs) = ByteString32 (BSL.take i bs)

append32 :: ByteString32 -> ByteString32 -> ByteString32
append32 (ByteString32 l) (ByteString32 r) = ByteString32 (BSL.append l r)

drop32 :: Int64 ->  ByteString32 -> ByteString32
drop32 i (ByteString32 bs) = ByteString32 (BSL.drop i bs)