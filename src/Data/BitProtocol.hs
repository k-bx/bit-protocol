{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Data.BitProtocol
  ( BitsVal(..)
  , encodeBS8
  , parseBS8
  , parseBS8Prefixed
  -- * helpers / nice things to have
  , numberToBits
  -- * internal
  , bitsValBiggerToCharUnsafe
  , word8sToIntegral
  , numToWord8Array
  , roundTo8
  , readBitValue
  , byteStringToBitsVal
  ) where

import Data.Bits
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import qualified Data.DList as DL
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural
import Test.QuickCheck (Arbitrary(..), arbitrarySizedNatural)

data BitsVal a = BitsVal
  { bvBitsNum :: Natural
  , bvVal :: a
  } deriving (Show, Eq, Generic)

instance Arbitrary a => Arbitrary (BitsVal a) where
  arbitrary = BitsVal <$> arbitrarySizedNatural <*> arbitrary

instance Num a => Semigroup (BitsVal a) where
  (BitsVal size1 val1) <> (BitsVal size2 val2) =
    BitsVal (size1 + size2) (val1 * 2 ^ size2 + val2)

-- | WARNING! Can overflow, so only concat on a small number of items
instance Integral a => Monoid (BitsVal a) where
  mempty = BitsVal 0 0

numToWord8Array :: (Integral a, Bits a) => BitsVal a -> [Word8]
numToWord8Array x' = go x'
  where
    go (BitsVal len _)
      | len <= 0 = []
    go (BitsVal len val)
      | len <= 8 = [fromIntegral val]
    go (BitsVal len val) =
      (fromIntegral (val `shiftR` (fromIntegral len - 8))) :
      go (BitsVal (len - 8) (val `mod` 2 ^ (len - 8)))

word8sToIntegral :: Integral a => [Word8] -> a
word8sToIntegral xs' = go xs' (length xs')
  where
    go [] _ = 0
    go (x:xs) len = fromIntegral x * (2 ^ ((len - 1) * 8)) + go xs (len - 1)

-- | Convert left 8 bits to a list of 'Word8', while giving a leftover
-- value). Assumes that the 'BitsVal' argument's length is more than
-- 8.
bitsValBiggerToCharUnsafe ::
     (Bits a, Integral a, Show a) => BitsVal a -> ([Word8], BitsVal a)
bitsValBiggerToCharUnsafe x =
  let word8arr = numToWord8Array x
      (word8arrHead, word8arrTail) =
        splitAt (fromIntegral (bvBitsNum x) `div` 8) word8arr
   in ( word8arrHead
      , BitsVal (bvBitsNum x `mod` 8) (word8sToIntegral word8arrTail))

roundTo8 :: (Integral a, Show a) => BitsVal a -> BitsVal a
roundTo8 (BitsVal 0 _val) = BitsVal 0 0
roundTo8 (BitsVal len val) =
  let newLen = len + (8 - (len `mod` 8))
      newVal = val * (2 ^ (newLen - len))
   in BitsVal newLen newVal

-- | Converts a list of chars into a bytestring via construction of
-- 8-bit chars. Pads with zeroes on the right if a sum is not divisible by 8.
encodeBS8 :: (Bits a, Integral a, Show a) => [BitsVal a] -> ByteString
encodeBS8 xs' = BB.toLazyByteString (go xs' DL.empty)
  where
    go ::
         (Bits a, Integral a, Show a)
      => [BitsVal a]
      -> DL.DList (BitsVal a)
      -> BB.Builder
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

byteStringToBitsVal :: Integral a => ByteString -> BitsVal a
byteStringToBitsVal inp =
  let bytes = BL.unpack inp
   in go bytes (BitsVal 0 0)
  where
    go [] acc = acc
    go (x:xs) (BitsVal len val) =
      go xs (BitsVal (len + 8) (val * (2 ^ (8 :: Int)) + fromIntegral x))

-- | Read a single 'BitsVal' from a 'BitsVal' which wasn't consumed
-- (part of a byte) and some 'ByteString' big enough to cover that
-- value
readBitValue ::
     (Bits a, Integral a, Show a)
  => Natural
  -> BitsVal a
  -> ByteString
  -> (BitsVal a, BitsVal a)
readBitValue numBits leftBv inp =
  let rightBv = byteStringToBitsVal inp
      inpBvFull = leftBv <> rightBv
      inpBvNumBitsOnly =
        BitsVal
          numBits
          (bvVal inpBvFull `shiftR`
           (fromIntegral (bvBitsNum inpBvFull - numBits)))
      inpBvLeftover =
        BitsVal
          (bvBitsNum inpBvFull - numBits)
          (bvVal inpBvFull `mod` 2 ^ (bvBitsNum inpBvFull - numBits))
   in (inpBvNumBitsOnly, inpBvLeftover)

-- | Parse a 'ByteString' by a given spec. Return the values consumed,
-- a leftover BitsVal (will be 'BitsVal 0 0' for fully-consumed byte,
-- something else for a half-consumed one) and a leftover 'ByteString'
-- tail.
parseBS8 ::
     (Bits a, Integral a, Show a)
  => [Natural]
  -> ByteString
  -> ([BitsVal a], BitsVal a, ByteString)
parseBS8 bitLengths input = parseBS8Prefixed bitLengths input (BitsVal 0 0)

-- | Parse a 'ByteString' which also has some non-consumed prefix with
-- a number of bits under 8 usually.
parseBS8Prefixed ::
     (Bits a, Integral a, Show a)
  => [Natural]
  -> ByteString
  -> BitsVal a
  -> ([BitsVal a], BitsVal a, ByteString)
parseBS8Prefixed bitLengths input prefix =
  let (bitVals, bvInp, rest) = go bitLengths prefix input DL.empty
   in (bitVals, bvInp, rest)
  where
    go [] bvInp inp acc = (DL.toList acc, bvInp, inp)
    go (x:xs) bvInp inp acc =
      let bytesNeeded =
            ((x - bvBitsNum bvInp) `div` 8) +
            (if (x - bvBitsNum bvInp) `mod` 8 == 0
               then 0
               else 1)
          (chunk, rest) = BL.splitAt (fromIntegral bytesNeeded) inp
          (bv, bvLeftover) = readBitValue x bvInp chunk
       in go xs bvLeftover rest (DL.snoc acc bv)

-- | Convert a number into a list of bools describing every bit.
numberToBits :: (Integral a, Bits a) => BitsVal a -> [Bool]
numberToBits (BitsVal len val) = map getBit [1 .. len']
  where
    len' = fromIntegral len
    getBit i = testBit val (len' - i)
