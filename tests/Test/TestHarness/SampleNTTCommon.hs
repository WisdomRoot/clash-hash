{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTTCommon
  ( -- Re-export shared types from SHAKECommon
    UpstreamStall (..),
    DownstreamBackpressure (..),
    ShakeTest (..),
    testLabel,
    makeBasicTest,
    makeVariableOutputTest,
    makeStallTest,
    makeBackpressureTest,
    makeCombinedTest,
    -- SampleNTT-specific types and functions
    SampleNTTParams (..),
    SampleNTTTopEntity,
    runSampleNTTTest,
    runSampleNTTHardware,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Clash.Sized.Vector qualified as V
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.Hspec (Expectation, shouldBe)
import Test.TestHarness.SHAKECommon (DownstreamBackpressure (..), ShakeTest (..), UpstreamStall (..), makeBackpressureTest, makeBasicTest, makeCombinedTest, makeStallTest, makeVariableOutputTest, testLabel)
import Test.TestHarness.StreamCommon
  ( bitListToBSHW,
    makeBackpressureSignal,
    wordToBits,
  )
import Prelude qualified as P

--------------------------------------------------------------------------------
-- SampleNTT-specific types
--------------------------------------------------------------------------------

type SampleNTTTopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 272) ->
  Signal System Bool ->
  Signal System (AXI4Stream 64, Bool)

data SampleNTTParams = SampleNTTParams
  { spBeatsPerBlock :: Int,
    spReference :: Int -> ByteString -> ByteString,
    spTopEntity :: SampleNTTTopEntity
  }

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

runSampleNTTTest :: SampleNTTParams -> ShakeTest -> Expectation
runSampleNTTTest params test = do
  let expected = spReference params (testOutputBytes test) (testMessage test)
      actual = runSampleNTTHardware params test
  BS.length actual `shouldBe` testOutputBytes test
  BS.length expected `shouldBe` testOutputBytes test
  actual `shouldBe` expected

runSampleNTTHardware :: SampleNTTParams -> ShakeTest -> ByteString
runSampleNTTHardware params test =
  let beatsPerBlock = spBeatsPerBlock params
      msgBV = bsToBV272 (testMessage test)
      msgSig = pure msgBV
      treadySignal = makeBackpressureSignal (testDownstreamBackpressure test)
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          msgSig
          treadySignal
      outputBytes = testOutputBytes test
      outputBeats = (outputBytes P.+ 7) `P.div` 8
      squeezesNeeded = (outputBeats P.+ beatsPerBlock - 1) `P.div` beatsPerBlock
      sampleCount =
        24
          P.+ squeezesNeeded P.* (beatsPerBlock P.+ 24)
          P.+ 200
      samples = sampleN @System sampleCount (bundle (output, treadySignal))
      validOutputs =
        [ tdata stream
          | ((stream, _), ready) <- samples,
            tvalid stream P.&& ready
        ]
      outputWordBits = P.concatMap wordToBits (P.take outputBeats validOutputs)
      resultBits = P.take (outputBytes P.* 8) outputWordBits
   in bitListToBSHW resultBits

bsToBV272 :: ByteString -> BitVector 272
bsToBV272 bs =
  let bytes = BS.unpack bs
      padded = P.take 34 (bytes P.++ P.repeat 0)
      vec :: Vec 34 (BitVector 8)
      vec = V.unsafeFromList (P.map (fromIntegral :: Word8 -> BitVector 8) padded)
   in pack vec
