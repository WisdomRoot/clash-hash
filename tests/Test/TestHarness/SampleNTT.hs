{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTT
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel,
  )
where

import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Data.Word (Word16, Word8)
import Hash.NonPipelined.SampleNTT qualified as SampleNTT
import Reference.SampleNTT qualified as Ref
import Test.Hspec (Expectation)
import Test.TestHarness.SHAKECommon
  ( ShakeParams (..),
    ShakeTest,
    runShakeHardware,
    runShakeTest,
  )
import Test.TestHarness.SHAKECommon qualified as Common
import Prelude

-- | SampleNTT always outputs 256 coefficients = 512 bytes = 64 beats
-- Input blocks are 21 beats (like SHAKE128) but output is fixed at 64 beats
sampleNTTParams :: ShakeParams
sampleNTTParams =
  ShakeParams
    { spBeatsPerBlock = 64,  -- 64 beats × 8 bytes = 512 bytes output
      spReference = referenceSampleNTT,
      spTopEntity = SampleNTT.topEntity
    }

-- | Reference implementation wrapper
-- SampleNTT always produces 256 coefficients regardless of requested output size
referenceSampleNTT :: Int -> ByteString -> ByteString
referenceSampleNTT _outputBytes input =
  let coeffVec = Ref.sampleNTT input
      coeffList = V.toList coeffVec
      bytes = concatMap word16ToBytesLE coeffList
   in BS.pack bytes
  where
    -- Little-endian encoding of Word16
    word16ToBytesLE :: Word16 -> [Word8]
    word16ToBytesLE w =
      [ fromIntegral (w Bits..&. 0xFF),
        fromIntegral (w `Bits.shiftR` 8)
      ]

runTest :: ShakeTest -> Expectation
runTest = runShakeTest sampleNTTParams

runHardware :: ShakeTest -> ByteString
runHardware = runShakeHardware sampleNTTParams

testLabel :: ShakeTest -> String
testLabel = Common.testLabel
