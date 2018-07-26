{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BitProtocol where

import Data.ByteString.Base64.URL (encode)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC8
import Data.ByteString.Lazy (ByteString)
import Data.Char (chr, intToDigit)
import Data.Int (Int8)
import Data.Int (Int64)
import GHC.Types (Nat)
import Numeric (showHex, showIntAtBase)
import Text.Printf

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

numToInt8Array :: Integral a => BitsVal a -> [Int8]
numToInt8Array x' = go x'
  where
    go (BitsVal len _)
      | len <= 0 = []
    go (BitsVal len val)
      | len <= 8 = [fromIntegral val]
    go (BitsVal len val) =
      (fromIntegral (val `div` (2 ^ (len - 8)))) :
      go (BitsVal (len - 8) (val `mod` 2 ^ (len - 8)))

int8sToIntegral :: Integral a => [Int8] -> a
int8sToIntegral xs' = go xs' (length xs')
  where
    go [] _ = 0
    go (x:xs) len = fromIntegral x * (2 ^ ((len - 1) * 8)) + go xs (len - 1)

-- | Convert left 8 bits to a list of 'Int8', while giving a leftover
-- value). Assumes that the 'BitsVal' argument's length is more than
-- 8.
bitsValBiggerToCharUnsafe :: Integral a => BitsVal a -> ([Int8], BitsVal a)
bitsValBiggerToCharUnsafe x =
  let int8arr = numToInt8Array x
      (int8arrHead, int8arrTail) = splitAt (bvBitsNum x `div` 8) int8arr
   in (int8arrHead, BitsVal (bvBitsNum x `mod` 8) (int8sToIntegral int8arrTail))

-- | Converts a list of chars into a bytestring via construction of
-- 8-bit chars. Pads with zeroes on the right if a sum is not divisible by 8.
bitsValsToBS8 :: (Integral a) => [BitsVal a] -> ByteString
bitsValsToBS8 xs' = BB.toLazyByteString (go xs' [])
  where
    -- TODO: convert prefix to difflist
    go :: Integral a => [BitsVal a] -> [BitsVal a] -> BB.Builder
    go [] prefix = "" -- TODO: encode prefix leftover
    go (x:xs) prefix =
      let bitsToConvert = sumOfBitsNum prefix + bvBitsNum x
          prefixWithX = prefix ++ [x]
       in if bitsToConvert < 8
            then go xs (prefix ++ [x])
            else let (int8s, bv) =
                       bitsValBiggerToCharUnsafe (mconcat prefixWithX)
                  in mconcat (map BB.int8 int8s) <> go xs [bv]
    sumOfBitsNum = sum . map bvBitsNum
