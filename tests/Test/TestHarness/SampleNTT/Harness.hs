{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTT.Harness
  ( UpstreamStall (..),
    DownstreamBackpressure (..),
    ShakeTest (..),
    testLabel,
    makeBasicTest,
    makeVariableOutputTest,
    makeStallTest,
    makeBackpressureTest,
    makeCombinedTest,
    SampleNTTParams (..),
    SampleNTTTopEntity,
    runSampleNTTTest,
    runSampleNTTHardware,
    unpackPython384Bytes,
    getTimingInfo,
    simulateTiming,
    runTest,
    runHardware,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.SampleNTT qualified as SampleNTT
import Data.ByteString (ByteString)
import Data.Word (Word16)
import Test.Hspec (Expectation, shouldBe)
import Test.TestHarness.SampleNTT.Common
  ( DownstreamBackpressure (..),
    ShakeTest (..),
    UpstreamStall (..),
    backpressurePattern,
    bsToBV272,
    externalSampleNTTPacked,
    getTimingInfo,
    makeBackpressureSignalRepeat,
    makeBackpressureTest,
    makeBasicTest,
    makeCombinedTest,
    makeStallTest,
    makeUpstreamValidSignal,
    makeVariableOutputTest,
    reverseBits12Word,
    sampleCountMargin,
    simulateTiming,
    testLabel,
    unpackPython384Bytes,
  )
import Prelude qualified as P

--------------------------------------------------------------------------------
-- SampleNTT-specific types
--------------------------------------------------------------------------------

type SampleNTTTopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 12, Bool)

data SampleNTTParams = SampleNTTParams
  { spReference :: ByteString -> [Word16],
    spTopEntity :: SampleNTTTopEntity
  }

--------------------------------------------------------------------------------
-- Default harness configuration
--------------------------------------------------------------------------------

sampleNTTParams :: SampleNTTParams
sampleNTTParams =
  SampleNTTParams
    { spReference = unpackPython384Bytes . externalSampleNTTPacked,
      spTopEntity = SampleNTT.topEntity
    }

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

runTest :: ShakeTest -> Expectation
runTest = runSampleNTTTest sampleNTTParams

runHardware :: ShakeTest -> [Word16]
runHardware = runSampleNTTHardware sampleNTTParams

runSampleNTTTest :: SampleNTTParams -> ShakeTest -> Expectation
runSampleNTTTest params test = do
  let expected = P.map reverseBits12Word (spReference params (testMessage test))
      actual = runSampleNTTHardware params test
  P.length actual `shouldBe` 256
  P.length expected `shouldBe` 256
  actual `shouldBe` expected

runSampleNTTHardware :: SampleNTTParams -> ShakeTest -> [Word16]
runSampleNTTHardware params test =
  let input = bsToBV272 (testMessage test)
      treadyPattern = backpressurePattern (testDownstreamBackpressure test)
      treadySignal = makeBackpressureSignalRepeat treadyPattern
      msgStreamSig = fmap (\v -> AXI4Stream input v False) msgValidSig
      inputSig = bundle (msgStreamSig, treadySignal)
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          inputSig
      msgReadySig = fmap snd output
      (msgValidSig, msgReadyPrevSig) =
        withClockResetEnable clockGen resetGen enableGen $
          let msgReadyPrevSig' = register True msgReadySig
              msgValidSig' = makeUpstreamValidSignal (testUpstreamStall test) msgReadyPrevSig'
           in (msgValidSig', msgReadyPrevSig')
      validityPattern = getTimingInfo (testMessage test)
      sampleCount =
        simulateTiming
          validityPattern
          (testUpstreamStall test)
          (testDownstreamBackpressure test)
          P.+ sampleCountMargin
      samples = sampleN @System sampleCount (bundle (output, treadySignal))
      validOutputs =
        [ tdata stream
          | ((stream, _), ready) <- samples,
            tvalid stream P.&& ready
        ]
      coeffs = P.map (P.fromIntegral . (unpack :: BitVector 12 -> Unsigned 12)) (P.take 256 validOutputs)
      readySamples = sampleN @System sampleCount (bundle (msgValidSig, msgReadyPrevSig))
      readyViolations = [() | (valid, readyPrev) <- readySamples, valid P.&& P.not readyPrev]
      handshakes = [() | (valid, readyPrev) <- readySamples, valid P.&& readyPrev]
   in if P.null readyViolations
        then
          if P.null handshakes
            then P.error "SampleNTT: MSG_TVALID never asserted after MSG_TREADY"
            else coeffs
        else P.error "SampleNTT: MSG_TVALID asserted without MSG_TREADY in previous cycle"
