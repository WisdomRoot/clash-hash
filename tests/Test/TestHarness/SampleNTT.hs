module Test.TestHarness.SampleNTT
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel,
  )
where

import Component.SampleNTT qualified as SampleNTT
import Data.ByteString (ByteString)
import Data.Word (Word16)
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

-- | SHAKE128 output length for SampleNTT harness: 384 bytes (3072 bits)
-- Input blocks are 21 beats (SHAKE128 rate) during squeeze
sampleNTTParams :: SampleNTTParams
sampleNTTParams =
  SampleNTTParams
    { spBeatsPerBlock = 21, -- SHAKE128 rate: 21 beats/absorb block (21*64 = 1344 bits)
      spReference = unpackPython384Bytes . externalSampleNTTPacked,
      spTopEntity = SampleNTT.topEntity
    }

-- | External reference implementation using kyber-py
-- Returns 384 packed bytes (256 coefficients × 12 bits packed as triplets)
externalSampleNTTPacked :: ByteString -> ByteString
externalSampleNTTPacked = callPythonReference ("reference" </> "kyber" </> "sample_ntt.py")

runTest :: ShakeTest -> Expectation
runTest = runSampleNTTTest sampleNTTParams

runHardware :: ShakeTest -> [Word16]
runHardware = runSampleNTTHardware sampleNTTParams
