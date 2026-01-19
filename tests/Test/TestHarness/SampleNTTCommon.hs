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
    unpackPython384Bytes,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word16)
import Test.Hspec (Expectation, shouldBe)
import Test.TestHarness.SHAKECommon (DownstreamBackpressure (..), ShakeTest (..), UpstreamStall (..), makeBackpressureTest, makeBasicTest, makeCombinedTest, makeStallTest, makeVariableOutputTest, testLabel)
import Test.TestHarness.StreamCommon
  ( bitListToWords,
    bsToBitListHW,
    feedInput,
    makeBackpressureSignal,
  )
import Prelude qualified as P

--------------------------------------------------------------------------------
-- SampleNTT-specific types
--------------------------------------------------------------------------------

type SampleNTTTopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream 64, Bool) ->
  Signal System (AXI4Stream 12, Bool)

data SampleNTTParams = SampleNTTParams
  { spBeatsPerBlock :: Int,
    spReference :: ByteString -> [Word16],
    spTopEntity :: SampleNTTTopEntity
  }

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

runSampleNTTTest :: SampleNTTParams -> ShakeTest -> Expectation
runSampleNTTTest params test = do
  let expected = spReference params (testMessage test)
      actual = runSampleNTTHardware params test
  -- Verify length first for better error messages
  P.length actual `shouldBe` 256
  P.length expected `shouldBe` 256
  -- Verify coefficient-wise equality
  actual `shouldBe` expected

runSampleNTTHardware :: SampleNTTParams -> ShakeTest -> [Word16]
runSampleNTTHardware params test =
  let beatsPerBlock = spBeatsPerBlock params
      inputBytes = BS.length (testMessage test)
      beats = (inputBytes P.+ 7) `P.div` 8
   in case fromJust (someNatVal (P.fromIntegral beats)) of
        SomeNat (_ :: Proxy beats') ->
          runHardwareKnown @beats' params test beats beatsPerBlock

runHardwareKnown ::
  forall beats.
  (KnownNat beats) =>
  SampleNTTParams ->
  ShakeTest ->
  Int ->
  Int ->
  [Word16]
runHardwareKnown params test beats beatsPerBlock =
  let inputBS = testMessage test
      inputBits = bsToBitListHW inputBS
      paddedBits = P.take (beats P.* 64) (inputBits P.++ P.repeat 0)
      messageWords = bitListToWords @beats beats paddedBits
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ feedInput @beats beatsPerBlock (testUpstreamStall test) messageWords
      treadySignal = makeBackpressureSignal (testDownstreamBackpressure test)
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          treadySignal
          inputStream
      -- Conservative sample count for 256 coefficients
      sampleCount =
        beats P.* 2
          P.+ 24 -- SHAKE permutation latency
          P.+ 256 P.* 3 -- 256 coefficients with potential gaps
          P.+ 200 -- Safety margin
      samples = sampleN @System sampleCount output
      -- Extract valid beats where handshake completed (tvalid && tready)
      validOutputs = [tdata stream | (stream, ready) <- samples, tvalid stream P.&& ready]
      -- Convert BitVector 12 to Word16, take exactly 256 coefficients
      coeffs = P.map bitVectorToWord16 (P.take 256 validOutputs)
   in coeffs
  where
    bitVectorToWord16 :: BitVector 12 -> Word16
    bitVectorToWord16 bv = P.fromIntegral (unpack bv :: Unsigned 12)

--------------------------------------------------------------------------------
-- Unpacking Python 384-byte format
--------------------------------------------------------------------------------

-- | Unpack Python's 384-byte format to 256 coefficients
-- Python packs two 12-bit coefficients into 3 bytes (128 triplets):
--   c0 = byte0 + 256 * (byte1 & 0x0F)  (bits 0-11)
--   c1 = (byte1 >> 4) + 16 * byte2      (bits 12-23)
-- Based on extractTwoCoeffs from Reference.SampleNTT
unpackPython384Bytes :: ByteString -> [Word16]
unpackPython384Bytes bs = go (BS.unpack bs)
  where
    go (b0 : b1 : b2 : rest) =
      let c0 = P.fromIntegral b0 P.+ 256 P.* (P.fromIntegral b1 .&. 0x0F)
          c1 = (P.fromIntegral b1 `shiftR` 4) P.+ 16 P.* P.fromIntegral b2
       in c0 : c1 : go rest
    go [] = []
    go _ = P.error "unpackPython384Bytes: Expected 384 bytes (multiple of 3)"
