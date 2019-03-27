{-# OPTIONS_GHC -O0 #-}
-- | Primitive names and functions for working with Plutus Core builtins.
module Language.PlutusTx.Builtins (
                                -- * Bytestring builtins
                                concatenate
                                , takeByteString
                                , dropByteString
                                , sha2_256
                                , sha3_256
                                , verifySignature
                                , equalsByteString
                                -- * Integer builtins
                                , addInteger
                                , subtractInteger
                                , multiplyInteger
                                , divideInteger
                                , remainderInteger
                                , greaterThanInteger
                                , greaterThanEqInteger
                                , lessThanInteger
                                , lessThanEqInteger
                                , equalsInteger
                                -- * Error
                                , error
                                -- * Strings
                                , String
                                , appendString
                                , emptyString
                                , charToString
                                -- * Tracing
                                , trace
                                ) where

import           Data.ByteString.Sized   (ByteString32, ByteString64)
import qualified Data.ByteString.Sized   as BSS
import           Prelude                 hiding (String, error)

import           Language.PlutusTx.Utils (mustBeReplaced)

-- TODO: resizing primitives? better handling of sizes?

concatenate :: ByteString32 -> ByteString32 -> ByteString32
concatenate = BSS.append32

takeByteString :: Int -> ByteString32 -> ByteString32
takeByteString i = BSS.take32 (fromIntegral i)

dropByteString :: Int -> ByteString32 -> ByteString32
dropByteString i = BSS.drop32 (fromIntegral i)

sha2_256 :: ByteString32 -> ByteString32
sha2_256 = mustBeReplaced

sha3_256 :: ByteString32 -> ByteString32
sha3_256 = mustBeReplaced

verifySignature :: ByteString32 -> ByteString32 -> ByteString64 -> Bool
verifySignature = mustBeReplaced

equalsByteString :: ByteString32 -> ByteString32 -> Bool
equalsByteString = (==)

addInteger :: Int -> Int -> Int
addInteger = (+)

subtractInteger :: Int -> Int -> Int
subtractInteger = (-)

multiplyInteger :: Int -> Int -> Int
multiplyInteger = (*)

divideInteger :: Int -> Int -> Int
divideInteger = div

remainderInteger :: Int -> Int -> Int
remainderInteger = rem

greaterThanInteger :: Int -> Int -> Bool
greaterThanInteger = (>)

greaterThanEqInteger :: Int -> Int -> Bool
greaterThanEqInteger = (>=)

lessThanInteger :: Int -> Int -> Bool
lessThanInteger = (<)

lessThanEqInteger :: Int -> Int -> Bool
lessThanEqInteger = (<=)

equalsInteger :: Int -> Int -> Bool
equalsInteger = (==)

error :: () -> a
error = mustBeReplaced

-- | An opaque type representing PLC strings.
data String

appendString :: String -> String -> String
appendString = mustBeReplaced

emptyString :: String
emptyString = mustBeReplaced

charToString :: Char -> String
charToString = mustBeReplaced

trace :: String -> ()
trace = mustBeReplaced
