{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}

module Data.WordArray
  ( WordArray(..)
  , Index(..)
  , toWordArray
  , readArray
  , writeArray
  , overIndex
  , unsafeIncr
  , iforWordArray
  , toList
  , toTuple
  , displayWordArray
  ) where

import           Data.Bits
import           Data.Functor
import           Data.Maybe           (fromMaybe)
import           Data.MonoTraversable
import           Data.Word
import           Numeric              (showHex)
import           Text.Show            (showListWith)

newtype WordArray = WordArray { fromWordArray :: Word64 }
  deriving (Eq, Ord)

instance Show WordArray where
    show = displayWordArray

type instance Element WordArray = Word8

newtype Index = Index { getIndex :: Int }
  deriving (Num, Eq, Ord)

instance Bounded Index where
  maxBound = 7
  minBound = 0

{-# INLINE toWordArray #-}
toWordArray :: Word64 -> WordArray
toWordArray = WordArray

displayWordArray :: WordArray -> String
displayWordArray wa = displayWordArrayS wa ""
  where
  displayHex x s = "0x" <> showHex x s
  displayWordArrayS = showListWith displayHex . toList

{-# INLINE toTuple #-}
toTuple :: WordArray -> (# Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray #)
toTuple (WordArray !w) =
  let
    !w0 = w
    !w1 = unsafeShiftR w0 8
    !w2 = unsafeShiftR w1 8
    !w3 = unsafeShiftR w2 8
    !w4 = unsafeShiftR w3 8
    !w5 = unsafeShiftR w4 8
    !w6 = unsafeShiftR w5 8
    !w7 = unsafeShiftR w6 8
  in
  (# fromIntegral w0
  ,  fromIntegral w1
  ,  fromIntegral w2
  ,  fromIntegral w3
  ,  fromIntegral w4
  ,  fromIntegral w5
  ,  fromIntegral w6
  ,  fromIntegral w7
  #)

{-# INLINE fromTuple #-}
fromTuple :: (# Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray, Element WordArray #) -> WordArray
fromTuple (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) =
    WordArray
      (                (fromIntegral w0)
      .|. unsafeShiftL (fromIntegral w1) 8
      .|. unsafeShiftL (fromIntegral w2) 16
      .|. unsafeShiftL (fromIntegral w3) 24
      .|. unsafeShiftL (fromIntegral w4) 32
      .|. unsafeShiftL (fromIntegral w5) 40
      .|. unsafeShiftL (fromIntegral w6) 48
      .|. unsafeShiftL (fromIntegral w7) 56
      )

{-# INLINE toList #-}
toList :: WordArray -> [Element WordArray]
toList !w =
  let (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) = toTuple w
  in [w0, w1, w2, w3, w4, w5, w6, w7]

{-# INLINE readArray #-}
readArray :: WordArray -> Index -> Element WordArray
readArray (WordArray !w) (Index !i) =
  let offset = 8*i
  in fromIntegral $ unsafeShiftR w offset

{-# INLINE writeArray #-}
writeArray :: WordArray -> Index -> Element WordArray -> WordArray
writeArray (WordArray !w) (Index !i) !w8 =
  let offset = 8*i
      w64 :: Word64
      w64 = unsafeShiftL (fromIntegral w8) offset
  in WordArray ((w .&. mask i) + w64)

{-# INLINE overIndex #-}
overIndex :: Index -> (Element WordArray -> Element WordArray) -> WordArray -> WordArray
overIndex !i f !w = writeArray w i $ f $ readArray w i

{-# INLINE unsafeIncr #-}
unsafeIncr :: Index -> WordArray -> WordArray
unsafeIncr (Index !i) (WordArray !w) =
  let offset = 8*i
      w64 :: Word64
      w64 = unsafeShiftL 1 offset
  in WordArray (w + w64)

{-# INLINE mask #-}
mask :: Int -> Word64
mask 7 = 0x00ffffffffffffff
mask 6 = 0xff00ffffffffffff
mask 5 = 0xffff00ffffffffff
mask 4 = 0xffffff00ffffffff
mask 3 = 0xffffffff00ffffff
mask 2 = 0xffffffffff00ffff
mask 1 = 0xffffffffffff00ff
mask 0 = 0xffffffffffffff00
mask _ = error "mask"

{-# INLINE iforWordArray #-}
iforWordArray :: Applicative f => WordArray -> (Int -> Element WordArray -> f ()) -> f ()
iforWordArray !w f =
  let (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) = toTuple w
  in   f 0 w0 *> f 1 w1 *> f 2 w2 *> f 3 w3 *> f 4 w4 *> f 5 w5 *> f 6 w6 *> f 7 w7

instance MonoFunctor WordArray where
  omap f w =
    let (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) = toTuple w
    in fromTuple (# f w0, f w1, f w2, f w3, f w4, f w5, f w6, f w7 #)

instance MonoFoldable WordArray where
  otoList = toList
  ofoldr f !b !w =
    let (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) = toTuple w
    in  f w0 $ f w1 $ f w2 $ f w3 $ f w4 $ f w5 $ f w6 $ f w7 b
  ofoldl' f z0 xs = ofoldr f' id xs z0
    where f' x k z = k $! f z x
  ofoldMap f = ofoldr (mappend . f) mempty
  onull _ = False
  oelem e = ofoldr (\a b -> a == e || b) False
  ofoldr1Ex f xs = fromMaybe
      (errorWithoutStackTrace "error in word-array ofoldr1Ex: empty array")
      (ofoldr mf Nothing xs)
    where
    mf x m = Just $ case m of
      Nothing -> x
      Just y  -> f x y
  ofoldl1Ex' f xs = fromMaybe
      (errorWithoutStackTrace "error in word-array ofoldr1Ex: empty array")
      (ofoldl' mf Nothing xs)
    where
    mf m y = Just $ case m of
      Nothing -> y
      Just x  -> f x y
  otraverse_ f !w =
    let (# !w0, !w1, !w2, !w3, !w4, !w5, !w6, !w7 #) = toTuple w
    in void (f w0 *> f w1 *> f w2 *> f w3 *> f w4 *> f w5 *> f w6 *> f w7)
