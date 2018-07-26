{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Data.BitProtocol where

import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL (encode)
import qualified Data.ByteString.Char8 as BC8
import Data.Char (chr, intToDigit)
import Data.Int (Int8)
import Data.Int (Int64)
import GHC.Types (Nat)
import Numeric (showHex, showIntAtBase)
import Text.Printf

-- -- * Task1. Produce a 'ByteString' which is a concatenation of the
-- -- byte-sequences, encoded in base64: 000001000001000001000001 ->
-- -- base64url -> "BBBB"
-- task1 :: IO ()
-- task1
--   -- 000001000001000001000001 == 00000100_00010000_01000001
--  = do
--   print $ encode $ BC8.pack "\x04\x10\x41"
--   -- printf "%06X\n" (0x041041::Int)
--   -- putStrLn $ showIntAtBase 16 intToDigit 0xC0 ""
--   -- we need to convert 0xc0 into '\xc0'
--   print $ encode $ BC8.pack ['\x04', '\x10', '\x41']
--   print $ encode $ BC8.pack (map chr [0x04, 0x10, 0x41])
-- * Task2. Produce a value, concatenated from four ints representing
-- a 6-bit value: [000001, 000001, 000001, 000001, 000001, 000001] ->
-- 000001000001000001000001000001000001
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

-- data BitsArray a = BitsArray
--   { baBitsNum :: Int
--   , baVal :: [a]
--   } deriving (Show, Eq)
-- | Assumes that the vsBitsNum is 8 exactly, otherwise the result is
-- not defined well
bitsValToCharUnsafe :: Integral a => BitsVal a -> Char
bitsValToCharUnsafe x = chr (fromIntegral (bvVal x))

numToInt8Array :: Integral a => BitsVal a -> [Int8]
numToInt8Array x' = reverse $ go x'
  where
    go (BitsVal len _)
      | len <= 0 = []
    go (BitsVal len val)
      | len <= 8 = [fromIntegral val]
    go (BitsVal len val) =
      (fromIntegral (val `mod` (2 ^ (len - 8)))) :
      go (BitsVal (len - 8) (val `div` 2 ^ (len - 8)))

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
bitsValsToBS8 xs' = BC8.pack (go xs' [])
  where
    go [] prefix = [] -- TODO: encode prefix leftover. TODO: convert to difflist
    go (x:xs) prefix =
      let bitsToConvert = sumOfBitsNum prefix + bvBitsNum x
          prefixWithX = prefix ++ [x]
       in if bitsToConvert < 8
            then go xs (prefix ++ [x])
            else let (int8s, bv) =
                       bitsValBiggerToCharUnsafe (mconcat prefixWithX)
                  in map (chr . fromIntegral) int8s ++ go xs [bv]
    sumOfBitsNum = sum . map bvBitsNum
-- task2 :: IO ()
-- task2 = do
--   let ints =
--         [ BitsVal 6 1
--         , BitsVal 6 1
--         , BitsVal 6 1
--         , BitsVal 6 1
--         ]
--   print $ bitsValsToBS8 ints
--   return ()
