{-# LANGUAGE OverloadedStrings #-}

module Test.SampleNTT (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary (..), Gen, forAll, vectorOf)
import Test.TestHarness.SampleNTT
  ( ShakeTest,
    runTest,
    testLabel,
  )
import Test.TestHarness.SHAKECommon
  ( ShakeTest (..),
    makeBackpressureTest,
    makeBasicTest,
    makeCombinedTest,
    makeStallTest,
  )
import Test.TestHarness.SHAKESamples
  ( backpressurePatternAggressive,
    backpressurePatternSimple,
    stallPatternAggressive,
    stallPatternModerate,
    stallPatternSimple,
  )
import Prelude

-- SampleNTT ALWAYS outputs 384 bytes (256 coefficients × 12 bits)
sampleNTTOutputBytes :: Int
sampleNTTOutputBytes = 384

-- | Create 34-byte seed for ML-KEM: 32-byte rho + i + j
makeSeed :: ByteString -> Word8 -> Word8 -> ByteString
makeSeed rho32 i j =
  let rho = BS.take 32 (rho32 <> BS.replicate 32 0) -- Ensure exactly 32 bytes
   in rho <> BS.pack [i, j]

-- | Test seeds (all 34 bytes)
seed1, seed2, seed3, seed4, seed5, seed6, seed7, seed8 :: ByteString
seed1 = makeSeed (BS.replicate 32 0x00) 0 0
seed2 = makeSeed (BS.replicate 32 0x01) 1 0
seed3 = makeSeed (BS.replicate 32 0x02) 0 1
seed4 = makeSeed (BS.replicate 32 0x03) 2 3
seed5 = makeSeed (BS.pack [0 .. 31]) 0 0
seed6 = makeSeed (BS.pack (take 32 (cycle [0x55, 0xAA]))) 1 1
seed7 = makeSeed (BS.pack (take 32 (cycle [116, 101, 115, 116]))) 2 2 -- "test" in ASCII
seed8 = makeSeed (BS.pack (take 32 (cycle [0xFF]))) 3 3

-- | QuickCheck generator for 34-byte seeds
genSampleNTTSeed :: Gen ByteString
genSampleNTTSeed = do
  rhoBytes <- vectorOf 32 arbitrary
  i <- arbitrary
  j <- arbitrary
  return $ BS.pack rhoBytes <> BS.pack [i, j]

-- | QuickCheck instance for SampleNTT test with 34-byte seeds
genSampleNTTTest :: Gen ShakeTest
genSampleNTTTest = do
  seed <- genSampleNTTSeed
  upstreamStall <- arbitrary
  downstreamBackpressure <- arbitrary
  return $
    ShakeTest
      { testMessage = seed,
        testOutputBytes = sampleNTTOutputBytes,
        testUpstreamStall = upstreamStall,
        testDownstreamBackpressure = downstreamBackpressure
      }

spec :: Spec
spec = describe "NonPipelined SampleNTT Tests" $ do
  basicTests
  stallTests
  backpressureTests
  propertyTests

-- | Basic functionality tests with 34-byte seeds
basicTests :: Spec
basicTests = describe "Basic functionality tests (34-byte seeds)" $ do
  mapM_
    runTestCase
    [ makeBasicTest seed1 sampleNTTOutputBytes,
      makeBasicTest seed2 sampleNTTOutputBytes,
      makeBasicTest seed3 sampleNTTOutputBytes,
      makeBasicTest seed4 sampleNTTOutputBytes,
      makeBasicTest seed5 sampleNTTOutputBytes,
      makeBasicTest seed6 sampleNTTOutputBytes,
      makeBasicTest seed7 sampleNTTOutputBytes,
      makeBasicTest seed8 sampleNTTOutputBytes
    ]

-- | Upstream stall handling with 34-byte seeds
stallTests :: Spec
stallTests = describe "Upstream stall handling (34-byte seeds)" $ do
  mapM_
    runTestCase
    [ makeStallTest seed1 sampleNTTOutputBytes stallPatternAggressive,
      makeStallTest seed2 sampleNTTOutputBytes stallPatternModerate,
      makeStallTest seed3 sampleNTTOutputBytes stallPatternSimple
    ]

-- | Downstream backpressure handling with 34-byte seeds
backpressureTests :: Spec
backpressureTests = describe "Downstream backpressure handling (34-byte seeds)" $ do
  mapM_
    runTestCase
    [ makeBackpressureTest seed4 sampleNTTOutputBytes backpressurePatternSimple,
      makeCombinedTest
        seed5
        sampleNTTOutputBytes
        stallPatternSimple
        backpressurePatternAggressive,
      makeCombinedTest
        seed6
        sampleNTTOutputBytes
        stallPatternAggressive
        backpressurePatternAggressive
    ]

-- | QuickCheck property tests with 34-byte seeds
propertyTests :: Spec
propertyTests = describe "QuickCheck property tests (34-byte seeds)" $ do
  it "correctly handles random 34-byte test cases" $ do
    forAll genSampleNTTTest runTest

runTestCase :: ShakeTest -> Spec
runTestCase test = it (testLabel test) $ runTest test
