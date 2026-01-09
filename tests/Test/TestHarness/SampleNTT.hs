module Test.TestHarness.SampleNTT
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel,
  )
where

import Clash.Prelude (unbundle)
import Data.ByteString (ByteString)
import Data.Word (Word16)
import Component.SampleNTT qualified as SampleNTT
import System.FilePath ((</>))
import Test.Hspec (Expectation)
import Test.TestHarness.ExternalReference (callPythonReference)
import Test.TestHarness.SampleNTTCommon
  ( SampleNTTParams (..),
    ShakeTest,
    runSampleNTTHardware,
    runSampleNTTTest,
    testLabel,
    unpackPython384Bytes,
  )
import Prelude

-- | SampleNTT always outputs 256 coefficients × 12 bits = 3072 bits = 384 bytes
-- Input blocks are 21 beats (like SHAKE128) but output is 256 coefficients as 12-bit beats
sampleNTTParams :: SampleNTTParams
sampleNTTParams =
  SampleNTTParams
    { spBeatsPerBlock = 21, -- SHAKE128 rate: 21 beats/absorb block (21*64 = 1344 bits)
      spReference = \seed -> unpackPython384Bytes (externalSampleNTTPacked seed),
      spTopEntity = \clk rst en treadySig inputPair ->
        let (msgSig, _) = unbundle inputPair
         in SampleNTT.topEntity clk rst en treadySig msgSig
    }

-- | External reference implementation using kyber-py
-- Returns 384 packed bytes (256 coefficients × 12 bits packed as triplets)
externalSampleNTTPacked :: ByteString -> ByteString
externalSampleNTTPacked = callPythonReference ("reference" </> "kyber" </> "sample_ntt.py")

runTest :: ShakeTest -> Expectation
runTest = runSampleNTTTest sampleNTTParams

runHardware :: ShakeTest -> [Word16]
runHardware = runSampleNTTHardware sampleNTTParams
