module Test.TestHarness.SampleNTT
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel,
  )
where

import Component.SampleNTT qualified as SampleNTT
import Data.ByteString (ByteString)
import Reference.Crypton qualified as Crypton
import Test.Hspec (Expectation)
import Test.TestHarness.SampleNTTCommon
  ( SampleNTTParams (..),
    ShakeTest,
    runSampleNTTHardware,
    runSampleNTTTest,
    testLabel,
  )

-- | SHAKE128 output length for SampleNTT harness: 384 bytes (3072 bits)
-- Input blocks are 21 beats (SHAKE128 rate) during squeeze
sampleNTTParams :: SampleNTTParams
sampleNTTParams =
  SampleNTTParams
    { spBeatsPerBlock = 21, -- SHAKE128 rate: 21 beats/absorb block (21*64 = 1344 bits)
      spReference = Crypton.shake128,
      spTopEntity = \clk rst en msgSig treadySig ->
        SampleNTT.topEntity clk rst en msgSig treadySig
    }

runTest :: ShakeTest -> Expectation
runTest = runSampleNTTTest sampleNTTParams

runHardware :: ShakeTest -> ByteString
runHardware = runSampleNTTHardware sampleNTTParams
