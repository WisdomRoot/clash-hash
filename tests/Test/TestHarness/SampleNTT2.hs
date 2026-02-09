{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTT2
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.SampleNTT2 qualified as SampleNTT2
import Data.ByteString (ByteString)
import Data.Word (Word16)
import Test.Hspec (Expectation, shouldBe)
import Test.TestHarness.SampleNTT.Common
  ( ShakeTest (..),
    backpressurePattern,
    bsToBV272,
    externalSampleNTTPacked,
    getTimingInfo,
    makeBackpressureSignalRepeat,
    makeUpstreamValidSignal,
    reverseBits12Word,
    sampleCountMargin,
    simulateTiming2,
    splitCoeffsN,
    testLabel,
    unpackPython384Bytes,
  )
import Prelude qualified as P

--------------------------------------------------------------------------------
-- SampleNTT2-specific types
--------------------------------------------------------------------------------

type SampleNTT2TopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)

data SampleNTT2Params = SampleNTT2Params
  { spReference :: ByteString -> [Word16],
    spTopEntity :: SampleNTT2TopEntity
  }

--------------------------------------------------------------------------------
-- Default harness configuration
--------------------------------------------------------------------------------

sampleNTT2Params :: SampleNTT2Params
sampleNTT2Params =
  SampleNTT2Params
    { spReference = unpackPython384Bytes . externalSampleNTTPacked,
      spTopEntity = SampleNTT2.topEntity
    }

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

runTest :: ShakeTest -> Expectation
runTest = runSampleNTT2Test sampleNTT2Params

runHardware :: ShakeTest -> [Word16]
runHardware = runSampleNTT2Hardware sampleNTT2Params

runSampleNTT2Test :: SampleNTT2Params -> ShakeTest -> Expectation
runSampleNTT2Test params test = do
  let expected = P.map reverseBits12Word (spReference params (testMessage test))
      actual = runSampleNTT2Hardware params test
  P.length actual `shouldBe` 256
  P.length expected `shouldBe` 256
  actual `shouldBe` expected

runSampleNTT2Hardware :: SampleNTT2Params -> ShakeTest -> [Word16]
runSampleNTT2Hardware params test =
  let input = bsToBV272 (testMessage test)
      treadyPattern = backpressurePattern (testDownstreamBackpressure test)
      treadySignal = makeBackpressureSignalRepeat treadyPattern
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          inputSig
      msgStreamSig = fmap (\v -> AXI4Stream input v False) msgValidSig
      inputSig = bundle (msgStreamSig, treadySignal)
      msgReadySig = fmap snd output
      (msgValidSig, msgReadyPrevSig) =
        withClockResetEnable clockGen resetGen enableGen $
          let msgReadyPrevSig' = register True msgReadySig
              msgValidSig' = makeUpstreamValidSignal (testUpstreamStall test) msgReadyPrevSig'
           in (msgValidSig', msgReadyPrevSig')
      validityPattern = getTimingInfo (testMessage test)
      sampleCount =
        simulateTiming2
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
      coeffs = P.take 256 (P.concatMap (splitCoeffsN @2) validOutputs)
      readySamples = sampleN @System sampleCount (bundle (msgValidSig, msgReadyPrevSig))
      readyViolations = [() | (valid, readyPrev) <- readySamples, valid P.&& P.not readyPrev]
      handshakes = [() | (valid, readyPrev) <- readySamples, valid P.&& readyPrev]
   in if P.null readyViolations
        then
          if P.null handshakes
            then P.error "SampleNTT2: MSG_TVALID never asserted after MSG_TREADY"
            else coeffs
        else P.error "SampleNTT2: MSG_TVALID asserted without MSG_TREADY in previous cycle"
