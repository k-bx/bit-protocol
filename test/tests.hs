{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.BitProtocol
import qualified Data.ByteString.Base64.URL.Lazy as B64URL
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "Semigroup/Monoid 000001 + 000001 == 000001000001" $ do
        mconcat [BitsVal 6 1, BitsVal 6 1] @?= BitsVal 12 0b000001000001
    , testCase "numToInt8Array simple" $ do
        numToInt8Array (BitsVal 12 0b000001000001) @?= [0b00000100, 0b0001]
    , testCase "int8sToIntegral simple" $ do int8sToIntegral [0b0001] @?= 1
    , testCase "bitsValBiggerToCharUnsafe simple" $ do
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
    , testCase "bitsValsToBS8 without encoding" $ do
        bitsValsToBS8
          [ BitsVal 4 0b0000
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
      do mconcat [BitsVal 6 0b000001, BitsVal 26 0b00111000010000010100010000]
     @?= BitsVal 32 0b00000100111000010000010100010000
    , testCase "division logic check" $ do
        0b00000100111000010000010100010000 `div`
          (2 ^ (32 - 8)) @?= 0b00000100
        0b00000100111000010000010100010000 `mod`
          (2 ^ (32 - 8)) @?= 0b111000010000010100010000
    , testCase "numToInt8Array simple test" $ do
        numToInt8Array (BitsVal 32 0b00000100111000010000010100010000) @?=
          [0b00000100, 0b11100001, 0b00000101, 0b00010000]
    , testCase "bitsValBiggerToCharUnsafe simple test" $ do
        bitsValBiggerToCharUnsafe
          (BitsVal 32 0b00000100111000010000010100010000) @?=
          ([0b00000100, 0b11100001, 0b00000101, 0b00010000], BitsVal 0 0)
    , testCase "bitsValsToBS8 without encoding for the GDPR subcase" $ do
        bitsValsToBS8
          [BitsVal 6 0b000001, BitsVal 26 0b00111000010000010100010000] @?=
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
    , testCase "GDPR subcase" $ do
        B64URL.encode
          (bitsValsToBS8
             [ BitsVal 6 0b000001
             , BitsVal 36 0b001110000100000101000100000000110010
             ]) @?=
          "BOEF"
    , testCase
        "GDPR example (see \"Example Vendor Consent String\" at https://github.com/InteractiveAdvertisingBureau/GDPR-Transparency-and-Consent-Framework/blob/master/Consent%20string%20and%20vendor%20list%20formats%20v1.1%20Final.md#example-vendor-consent-string-)" $ do
        let version = BitsVal 6 0b000001
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
            expectedResult = "BOEFEAyOEFEAyAHABDENAI4AAAB9vABAASA"
        B64URL.encode
          (bitsValsToBS8
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
    -- , testCase
    --     "GDPR example from https://github.com/InteractiveAdvertisingBureau/Consent-String-SDK-Java/blob/master/src/test/java/com/iab/gdpr/consent/VendorConsentEncoderTest.java" $ do
    --     let version = BitsVal 6 0b000011
    --         created = BitsVal 36 0b001110001110110011010000101000000000
    --         lastUpdated = BitsVal 36 0b001110001110110011010000101000000000
    --         cmpId = BitsVal 12 0b000000001111
    --         cmpVersion = BitsVal 12 0b000000000101
    --         consentScreen = BitsVal 6 0b010010
    --         consentLanguage = BitsVal 12 0b000100001101
    --         vendorListVersion = BitsVal 12 0b000010010110
    --         purposesAllowed = BitsVal 24 0b111110000000001000000001
    --         maxVendorId = BitsVal 16 0b0000000000100000
    --         encodingType = BitsVal 1 0
    --         vendorBits = BitsVal 16 0b0000000000100000
    --         expectedResult = "BOOlLqOOOlLqTABABAENAk-AAAAXx799uzGvrf3nW8_39P3g_7_O3_7m_-zzV48_lrQV1yPAUCgA"
    --     B64URL.encode
    --       (bitsValsToBS8
    --          [ version
    --          , created
    --          , lastUpdated
    --          , cmpId
    --          , cmpVersion
    --          , consentScreen
    --          , consentLanguage
    --          , vendorListVersion
    --          , purposesAllowed
    --          , maxVendorId
    --          , encodingType
    --          , vendorBits
    --          ]) @?=
    --       expectedResult
    ]

main :: IO ()
main = defaultMain tests
