{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.BitProtocol
import qualified Data.ByteString.Base64.URL as B64URL
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "Semigroup/Monoid 000001 + 000001 == 000001000001" $ do
        mconcat [BitsVal 6 1, BitsVal 6 1] @?= BitsVal 12 0b000001000001
    , testCase
        "numToInt8Array simple" $ do
          numToInt8Array (BitsVal 12 0b000001000001) @?= [0b00000100, 0b0001]
    , testCase "int8sToIntegral simple" $ do
        int8sToIntegral [0b0001] @?= 1
    , testCase
        "bitsValBiggerToCharUnsafe simple" $ do
        bitsValBiggerToCharUnsafe (BitsVal 12 0b000001000001) @?=
          ([0b00000100], BitsVal 4 0b0001)
    , testCase "Readme example" $ do
        let age = 29
            fav = 12
            lucky = 13
            rand = 14
        B64URL.encode
          (bitsValsToBS8
             [BitsVal 6 age, BitsVal 7 fav, BitsVal 5 lucky, BitsVal 6 rand]) @?=
          "dGNO"
    ]

main :: IO ()
main = defaultMain tests
