module Test.TestHarness.SampleNTT
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel,
  )
where

import Clash.Prelude (unbundle)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Component.SampleNTT qualified as SampleNTT
import System.FilePath ((</>))
import Test.Hspec (Expectation)
import Test.TestHarness.ExternalReference (callPythonReference)
import Test.TestHarness.SHAKECommon
  ( ShakeParams (..),
    ShakeTest,
    runShakeHardware,
    runShakeTest,
  )
import Test.TestHarness.SHAKECommon qualified as Common
import Prelude

-- | SampleNTT always outputs 256 coefficients × 12 bits = 3072 bits = 384 bytes = 48 beats
-- Input blocks are 21 beats (like SHAKE128) but output is fixed at 48 beats
sampleNTTParams :: ShakeParams
sampleNTTParams =
  ShakeParams
    { spBeatsPerBlock = 21, -- SHAKE128 rate: 21 beats/absorb block (21*64 = 1344 bits)
      spReference = \outBytes seed -> BS.take outBytes (externalSampleNTT seed),
      spTopEntity = \clk rst en treadySig inputPair ->
        let (msgSig, _) = unbundle inputPair
         in SampleNTT.topEntity clk rst en treadySig msgSig
    }

-- | External reference implementation using kyber-py
-- SampleNTT always produces 256 coefficients × 12 bits = 384 bytes regardless of requested output size
externalSampleNTT :: ByteString -> ByteString
externalSampleNTT = callPythonReference ("reference" </> "kyber" </> "sample_ntt.py")

runTest :: ShakeTest -> Expectation
runTest = runShakeTest sampleNTTParams

runHardware :: ShakeTest -> ByteString
runHardware = runShakeHardware sampleNTTParams

testLabel :: ShakeTest -> String
testLabel = Common.testLabel
