{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTT512
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.SampleNTT512 qualified as SampleNTT512
import Data.ByteString (ByteString)
import Data.Word (Word16)
import Test.Hspec (Expectation, shouldBe)
import Test.TestHarness.SampleNTT.Common
  ( ShakeTest (..),
    backpressurePattern,
    bsToBV272Normal,
    externalSampleNTTPacked,
    getTimingInfo,
    makeBackpressureSignalRepeat,
    makeUpstreamValidSignal,
    sampleCountMargin,
    simulateTiming2,
    splitCoeffsN,
    testLabel,
    unpackPython384Bytes,
  )
import Prelude qualified as P

--------------------------------------------------------------------------------
-- SampleNTT512-specific types
--------------------------------------------------------------------------------

type SampleNTT512TopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)

data SampleNTT512Params = SampleNTT512Params
  { spReference :: ByteString -> [Word16],
    spTopEntity :: SampleNTT512TopEntity
  }

--------------------------------------------------------------------------------
-- Default harness configuration
--------------------------------------------------------------------------------

sampleNTT512Params :: SampleNTT512Params
sampleNTT512Params =
  SampleNTT512Params
    { spReference = unpackPython384Bytes . externalSampleNTTPacked,
      spTopEntity = SampleNTT512.topEntity
    }

sampleCountMargin512 :: Int
sampleCountMargin512 = 20

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

runTest :: ShakeTest -> Expectation
runTest = runSampleNTT512Test sampleNTT512Params

runHardware :: ShakeTest -> [Word16]
runHardware = runSampleNTT512Hardware sampleNTT512Params

runSampleNTT512Test :: SampleNTT512Params -> ShakeTest -> Expectation
runSampleNTT512Test params test = do
  let expected = spReference params (testMessage test)
      actual = runSampleNTT512Hardware params test
  P.length actual `shouldBe` 256
  P.length expected `shouldBe` 256
  actual `shouldBe` expected

runSampleNTT512Hardware :: SampleNTT512Params -> ShakeTest -> [Word16]
runSampleNTT512Hardware params test =
  let msgBV = bsToBV272Normal (testMessage test)
      treadyPattern = backpressurePattern (testDownstreamBackpressure test)
      treadySignal = makeBackpressureSignalRepeat treadyPattern
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          inputSig
      msgStreamSig = fmap (\v -> AXI4Stream msgBV v False) msgValidSig
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
          P.+ sampleCountMargin512
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
            then P.error "SampleNTT512: MSG_TVALID never asserted after MSG_TREADY"
            else coeffs
        else P.error "SampleNTT512: MSG_TVALID asserted without MSG_TREADY in previous cycle"
