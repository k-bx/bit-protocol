{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.BitProtocol
import qualified Data.ByteString.Base64.URL.Lazy as B64URL
import GHC.Natural
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "Semigroup/Monoid 000001 + 000001 == 000001000001" $ do
        mconcat [BitsVal 6 (1 :: Int), BitsVal 6 1] @?=
          BitsVal 12 0b000001000001
    , testCase "numToWord8Array simple" $ do
        numToWord8Array (BitsVal 12 (0b000001000001 :: Int)) @?=
          [0b00000100, 0b0001]
    , testCase "word8sToIntegral simple" $ do
        word8sToIntegral [0b0001] @?= (1 :: Int)
    , testCase "bitsValBiggerToCharUnsafe simple" $ do
        bitsValBiggerToCharUnsafe (BitsVal 12 (0b000001000001 :: Int)) @?=
          ([0b00000100], BitsVal 4 0b0001)
    , testCase "Readme example" $ do
        let age = 29 :: Int
            fav = 12
            lucky = 13
            rand = 14
        B64URL.encode
          (encodeBS8
             [BitsVal 6 age, BitsVal 7 fav, BitsVal 5 lucky, BitsVal 6 rand]) @?=
          "dGNO"
    , testCase "encodeBS8 without encoding" $ do
        encodeBS8
          [ BitsVal 4 (0b0000 :: Int)
          , BitsVal 4 0b0100
          , BitsVal 4 0b1110
          , BitsVal 4 0b0001
          , BitsVal 4 0b0000
          , BitsVal 4 0b0101
          , BitsVal 4 0b0001
          , BitsVal 4 0b0000
          ] @?=
          "\x04\xE1\x05\x10"
    , testCase "mconcat simple test" $
      do mconcat
           [ BitsVal 6 0b000001
           , BitsVal 26 (0b00111000010000010100010000 :: Int)
           ]
     @?= BitsVal 32 0b00000100111000010000010100010000
    , testCase "division logic check" $ do
        (0b00000100111000010000010100010000 :: Int) `div`
          (2 ^ (32 - (8 :: Int))) @?= 0b00000100
        0b00000100111000010000010100010000 `mod`
          (2 ^ (32 - (8 :: Int))) @?= (0b111000010000010100010000 :: Int)
    , testCase "numToWord8Array simple test" $ do
        numToWord8Array (BitsVal 32 (0b00000100111000010000010100010000 :: Int)) @?=
          [0b00000100, 0b11100001, 0b00000101, 0b00010000]
    , testCase "bitsValBiggerToCharUnsafe simple test" $ do
        bitsValBiggerToCharUnsafe
          (BitsVal 32 (0b00000100111000010000010100010000 :: Int)) @?=
          ([0b00000100, 0b11100001, 0b00000101, 0b00010000], BitsVal 0 0)
    , testCase "encodeBS8 without encoding for the GDPR subcase" $ do
        encodeBS8
          [BitsVal 6 (0b000001 :: Int), BitsVal 26 0b00111000010000010100010000] @?=
          "\x04\xE1\x05\x10"
    , testCase "base64url reassurance" $
        -- 000001 001110000100000101000100000000110010 001110000100000101000100000000110010 ...
        -- 000001001110000100000101000100000000110010001110000100000101000100000000110010 ...
        -- 000001_001110_000100_000101_000100_000000_110010_001110_000100_000101_000100_000000_110010 ...
        -- 1 14 4 5 4 0 50 ...
        -- B O E F E A y ...
        -- 0000_0100_1110_0001_0000_0101_0001_0000_0000_1100_1000_1110_0001_0000_0101_0001_0000_0000_1100_10 ...
        -- 04E105100C ...
       do B64URL.encode "\x04\xE1\x05\x10\x0C" @?= "BOEFEAw="
    , testCase "roundTo8 simple" $ do
        roundTo8 (BitsVal 6 0b000001) @?= BitsVal 8 (0b00000100 :: Int)
    , testCase "roundTo8 zero" $ do
        roundTo8 (BitsVal 0 0) @?= BitsVal 0 (0 :: Int)
    , testCase "GDPR subcase" $ do
        B64URL.encode
          (encodeBS8
             [ BitsVal 6 (0b000001 :: Int)
             , BitsVal 36 0b001110000100000101000100000000110010
             ]) @?=
          "BOEFEAyA"
    , testCase
        "GDPR example (see \"Example Vendor Consent String\" at https://github.com/InteractiveAdvertisingBureau/GDPR-Transparency-and-Consent-Framework/blob/master/Consent%20string%20and%20vendor%20list%20formats%20v1.1%20Final.md#example-vendor-consent-string-)" $ do
        let version = BitsVal 6 (0b000001 :: Int)
            created = BitsVal 36 0b001110000100000101000100000000110010
            lastUpdated = BitsVal 36 0b001110000100000101000100000000110010
            cmpId = BitsVal 12 0b000000000111
            cmpVersion = BitsVal 12 0b000000000001
            consentScreen = BitsVal 6 0b000011
            consentLanguage = BitsVal 12 0b000100001101
            vendorListVersion = BitsVal 12 0b000000001000
            purposesAllowed = BitsVal 24 0b111000000000000000000000
            maxVendorId = BitsVal 16 0b0000011111011011
            encodingType = BitsVal 1 1
            defaultConsent = BitsVal 1 1
            numEntries = BitsVal 12 0b000000000001
            singleOrRange = BitsVal 1 0
            singleVendorId = BitsVal 16 0b0000000000001001
            expectedResult = "BOEFEAyOEFEAyAHABDENAI4AAAB9vABAASA="
        B64URL.encode
          (encodeBS8
             [ version
             , created
             , lastUpdated
             , cmpId
             , cmpVersion
             , consentScreen
             , consentLanguage
             , vendorListVersion
             , purposesAllowed
             , maxVendorId
             , encodingType
             , defaultConsent
             , numEntries
             , singleOrRange
             , singleVendorId
             ]) @?=
          expectedResult
    , testCase "Parsing a single bit" $ do
        (parseBS8Prefixed
           [1]
           "\NUL@\SOH "
           BitsVal {bvBitsNum = 4, bvVal = (12 :: Natural)}) @?=
          ([BitsVal {bvBitsNum = 1, bvVal = 1}],BitsVal {bvBitsNum = 3, bvVal = 4},"\NUL@\SOH ")
    , testCase "byteStringToBitsVal simple" $ do
        byteStringToBitsVal "\x04\xE1\xe05\x10" @?=
          BitsVal 32 (0b00000100111000010000010100010000 :: Int)
    , testCase
        "readBitValue reading 6 bits from \"010 + 11111111\" gives \"010111\" and a leftover \"11111\"" $ do
        readBitValue 6 (BitsVal 3 (0b010 :: Int)) "\xFF" @?=
          (BitsVal 6 0b010111, BitsVal 5 0b11111)
    , testCase "parseBS8 simple parse" $ do
        parseBS8 [6, 26] "\x04\xE1\x05\x10" @?=
          ( [ BitsVal 6 (0b000001 :: Int)
            , BitsVal 26 0b00111000010000010100010000
            ]
          , BitsVal 0 0
          , "")
    , testCase "parseBS8 from readme" $ do
        let (Right bs) = B64URL.decode "dGNO"
        parseBS8 [6, 7, 5, 6] bs @?=
          ( [BitsVal 6 (29 :: Int), BitsVal 7 12, BitsVal 5 13, BitsVal 6 14]
          , BitsVal 0 0
          , "")
    , testCase "quickcheck failure simplified-02" $ do
        readBitValue 20 (BitsVal 7 (0 :: Int)) "\NUL\DLE\ENQ\NUL" @?=
          (BitsVal 20 2, BitsVal 19 1280)
    , testCase "quickcheck failure simplified-01" $
      do parseBS8
           @Int
           [17, 20, 11, 15, 19, 13, 12, 13, 10]
           "\NUL\EOT\128\NUL\DLE\ENQ\NUL\STX\NUL\NUL\128\STX\NUL\128\STX\SOH\128"
     @?= ( [ BitsVal {bvBitsNum = 17, bvVal = 9}
           , BitsVal {bvBitsNum = 20, bvVal = 2}
           , BitsVal {bvBitsNum = 11, bvVal = 5}
           , BitsVal {bvBitsNum = 15, bvVal = 1}
           , BitsVal {bvBitsNum = 19, bvVal = 2}
           , BitsVal {bvBitsNum = 13, bvVal = 1}
           , BitsVal {bvBitsNum = 12, bvVal = 4}
           , BitsVal {bvBitsNum = 13, bvVal = 2}
           , BitsVal {bvBitsNum = 10, bvVal = 6}
           ]
         , BitsVal {bvBitsNum = 6, bvVal = 0}
         , "")
    , testCase "encode . decode = id from QuickCheck failure" $ do
        let xs =
              [ BitsVal 17 (9 :: Int)
              , BitsVal 20 2
              , BitsVal 11 5
              , BitsVal 15 1
              , BitsVal 19 2
              , BitsVal 13 1
              , BitsVal 12 4
              , BitsVal 13 2
              , BitsVal 10 6
              ]
            bitNums = map bvBitsNum xs
            (res, _, _) = parseBS8 @Int bitNums (encodeBS8 xs)
        res @?= xs
    , testCase "encode . decode = id from QuickCheck failure-02" $ do
        let xs :: [BitsVal Int]
            xs = [BitsVal 12 4, BitsVal 16 2]
            bitNums = [12, 16]
            (res, _, _) = parseBS8 bitNums (encodeBS8 xs)
        res @?= xs
    , QC.testProperty "encode . decode = id" $ \(xs' :: [BitsVal (Positive Integer)]) ->
        let xs =
              map (\(BitsVal len (Positive val)) -> BitsVal (len + 10) val) xs'
            bitNums = map bvBitsNum xs
            (res, _, _) = parseBS8 bitNums (encodeBS8 xs)
         in res == xs
    , testCase "numberToBits simple" $ do
        numberToBits (BitsVal 6 (0b010110 :: Int)) @?=
          [False, True, False, True, True, False]
    ]

main :: IO ()
main = defaultMain tests
