module Test.TestHarness.SampleNTTSamples
  ( -- Constants
    sampleNTTOutputBytes,
    -- Seed creation and fixtures
    makeSeed,
    seed1,
    seed2,
    seed3,
    seed4,
    seed5,
    seed6,
    seed7,
    seed8,
    seedBlockBoundary1,
    -- Test case arrays
    basicSeedCases,
    stallSeedCases,
    backpressureSeedCases,
    combinedSeedCases,
    -- QuickCheck generators
    genSampleNTTSeed,
    genSampleNTTTest,
    -- Re-export stall/backpressure patterns from SHAKE
    stallPatternSimple,
    stallPatternModerate,
    stallPatternAggressive,
    backpressurePatternSimple,
    backpressurePatternModerate,
    backpressurePatternAggressive,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Prelude
import Test.QuickCheck (Arbitrary (..), Gen, elements, frequency, vectorOf)
import Test.TestHarness.SHAKECommon
  ( DownstreamBackpressure (..),
    ShakeTest (..),
    UpstreamStall (..),
    makeBackpressureTest,
    makeBasicTest,
    makeCombinedTest,
    makeStallTest,
  )
import Test.TestHarness.SHAKESamples
  ( backpressurePatternAggressive,
    backpressurePatternModerate,
    backpressurePatternSimple,
    stallPatternAggressive,
    stallPatternModerate,
    stallPatternSimple,
  )

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | SampleNTT always outputs 384 bytes (256 coefficients × 12 bits packed)
sampleNTTOutputBytes :: Int
sampleNTTOutputBytes = 384

--------------------------------------------------------------------------------
-- Seed creation helper
--------------------------------------------------------------------------------

-- | Create 34-byte seed for ML-KEM SampleNTT: 32-byte rho + i + j
makeSeed :: ByteString -> Word8 -> Word8 -> ByteString
makeSeed rho32 i j =
  let rho = BS.take 32 (rho32 <> BS.replicate 32 0) -- Ensure exactly 32 bytes
   in rho <> BS.pack [i, j]

--------------------------------------------------------------------------------
-- Seed fixtures (all 34 bytes)
--------------------------------------------------------------------------------

seed1, seed2, seed3, seed4, seed5, seed6, seed7, seed8 :: ByteString
seed1 = makeSeed (BS.replicate 32 0x00) 0 0 -- All zeros
seed2 = makeSeed (BS.replicate 32 0x01) 1 0 -- All ones, different indices
seed3 = makeSeed (BS.replicate 32 0x02) 0 1 -- All twos, swapped indices
seed4 = makeSeed (BS.replicate 32 0x03) 2 3 -- All threes, larger indices
seed5 = makeSeed (BS.pack [0 .. 31]) 0 0 -- Sequential bytes
seed6 = makeSeed (BS.pack (take 32 (cycle [0x55, 0xAA]))) 1 1 -- Alternating pattern
seed7 = makeSeed (BS.pack (take 32 (cycle [116, 101, 115, 116]))) 2 2 -- "test" in ASCII
seed8 = makeSeed (BS.pack (replicate 32 0xFF)) 3 3 -- All ones

-- Regression seed: validity pattern lands exactly on a 112-candidate block boundary,
-- triggering the spurious extra-gap bug in the reference model.
seedBlockBoundary1 :: ByteString
seedBlockBoundary1 = BS.pack [0x00,0x01,0x01,0x01,0x00,0x00,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x01,0x01,0x01,0x01,0x00,0x01,0x01,0x00,0x01,0x00,0x01,0x01,0x00,0x00,0x01,0x00,0x01,0x01,0x01,0x00,0x00]

--------------------------------------------------------------------------------
-- Test case arrays
--------------------------------------------------------------------------------

basicSeedCases :: [ShakeTest]
basicSeedCases =
  [ makeBasicTest seed1 sampleNTTOutputBytes,
    makeBasicTest seed2 sampleNTTOutputBytes,
    makeBasicTest seed3 sampleNTTOutputBytes,
    makeBasicTest seed4 sampleNTTOutputBytes,
    makeBasicTest seed5 sampleNTTOutputBytes,
    makeBasicTest seed6 sampleNTTOutputBytes,
    makeBasicTest seed7 sampleNTTOutputBytes,
    makeBasicTest seed8 sampleNTTOutputBytes,
    makeBasicTest seedBlockBoundary1 sampleNTTOutputBytes
  ]

stallSeedCases :: [ShakeTest]
stallSeedCases =
  [ makeStallTest seed1 sampleNTTOutputBytes stallPatternAggressive,
    makeStallTest seed2 sampleNTTOutputBytes stallPatternModerate,
    makeStallTest seed3 sampleNTTOutputBytes stallPatternSimple
  ]

backpressureSeedCases :: [ShakeTest]
backpressureSeedCases =
  [ makeBackpressureTest seed4 sampleNTTOutputBytes backpressurePatternSimple
  ]

combinedSeedCases :: [ShakeTest]
combinedSeedCases =
  [ makeCombinedTest
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

--------------------------------------------------------------------------------
-- QuickCheck generators
--------------------------------------------------------------------------------

-- | Generator for random 34-byte SampleNTT seeds
genSampleNTTSeed :: Gen ByteString
genSampleNTTSeed = do
  rhoBytes <- vectorOf 32 arbitrary
  i <- arbitrary
  j <- arbitrary
  return $ BS.pack rhoBytes <> BS.pack [i, j]

-- | Generator for random SampleNTT test cases
genSampleNTTTest :: Gen ShakeTest
genSampleNTTTest = do
  seed <- genSampleNTTSeed
  upstreamStall <-
    frequency
      [ (3, pure NoUpstreamStall),
        (1, UpstreamStall <$> elements [stallPatternSimple, stallPatternModerate, stallPatternAggressive])
      ]
  downstreamBackpressure <-
    frequency
      [ (3, pure NoDownstreamBackpressure),
        (1, DownstreamBackpressure <$> elements [backpressurePatternSimple, backpressurePatternModerate, backpressurePatternAggressive])
      ]
  return $
    ShakeTest
      { testMessage = seed,
        testOutputBytes = sampleNTTOutputBytes,
        testUpstreamStall = upstreamStall,
        testDownstreamBackpressure = downstreamBackpressure
      }
