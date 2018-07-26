{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BitProtocol
  ( BitsVal(..)
  , bitsValsToBS8
  -- * internal
  , bitsValBiggerToCharUnsafe
  , word8sToIntegral
  , numToWord8Array
  , roundTo8
  ) where

import qualified Data.ByteString.Builder as BB
import Data.ByteString.Lazy (ByteString)
import qualified Data.DList as DL
import Data.Word (Word8)

data BitsVal a = BitsVal
  { bvBitsNum :: Int
  , bvVal :: a
  } deriving (Show, Eq)

instance Num a => Semigroup (BitsVal a) where
  (BitsVal size1 val1) <> (BitsVal size2 val2) =
    BitsVal (size1 + size2) (val1 * 2 ^ size2 + val2)

-- | WARNING! Can overflow, so only concat on a small number of items
instance Integral a => Monoid (BitsVal a) where
  mempty = BitsVal 0 0

numToWord8Array :: Integral a => BitsVal a -> [Word8]
numToWord8Array x' = go x'
  where
    go (BitsVal len _)
      | len <= 0 = []
    go (BitsVal len val)
      | len <= 8 = [fromIntegral val]
    go (BitsVal len val) =
      (fromIntegral (val `div` (2 ^ (len - 8)))) :
      go (BitsVal (len - 8) (val `mod` 2 ^ (len - 8)))

word8sToIntegral :: Integral a => [Word8] -> a
word8sToIntegral xs' = go xs' (length xs')
  where
    go [] _ = 0
    go (x:xs) len = fromIntegral x * (2 ^ ((len - 1) * 8)) + go xs (len - 1)

-- | Convert left 8 bits to a list of 'Word8', while giving a leftover
-- value). Assumes that the 'BitsVal' argument's length is more than
-- 8.
bitsValBiggerToCharUnsafe :: Integral a => BitsVal a -> ([Word8], BitsVal a)
bitsValBiggerToCharUnsafe x =
  let word8arr = numToWord8Array x
      (word8arrHead, word8arrTail) = splitAt (bvBitsNum x `div` 8) word8arr
   in ( word8arrHead
      , BitsVal (bvBitsNum x `mod` 8) (word8sToIntegral word8arrTail))

roundTo8 :: Integral a => BitsVal a -> BitsVal a
roundTo8 (BitsVal 0 _val) = BitsVal 0 0
roundTo8 (BitsVal len val) =
  let newLen = len + (8 - (len `mod` 8))
      newVal = val * (2 ^ (newLen - len))
   in BitsVal newLen newVal

-- | Converts a list of chars into a bytestring via construction of
-- 8-bit chars. Pads with zeroes on the right if a sum is not divisible by 8.
bitsValsToBS8 :: (Integral a) => [BitsVal a] -> ByteString
bitsValsToBS8 xs' = BB.toLazyByteString (go xs' DL.empty)
  where
    go :: Integral a => [BitsVal a] -> DL.DList (BitsVal a) -> BB.Builder
    go [] prefix =
      let v = mconcat (DL.toList prefix)
          newV = roundTo8 v
       in mconcat (map BB.word8 (fst (bitsValBiggerToCharUnsafe newV)))
    go (x:xs) prefix =
      let bitsToConvert = sumOfBitsNum prefix + bvBitsNum x
          prefixWithX = DL.snoc prefix x
       in if bitsToConvert < 8
            then go xs prefixWithX
            else let (word8s, bv) =
                       bitsValBiggerToCharUnsafe
                         (mconcat (DL.toList prefixWithX))
                  in mconcat (map BB.word8 word8s) <> go xs (DL.singleton bv)
    sumOfBitsNum = sum . DL.map bvBitsNum
