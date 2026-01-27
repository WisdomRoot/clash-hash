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
import Data.Word (Word16)
import Test.Hspec (Expectation, shouldBe)
import Test.TestHarness.SHAKECommon (DownstreamBackpressure (..), ShakeTest (..), UpstreamStall (..), makeBackpressureTest, makeBasicTest, makeCombinedTest, makeStallTest, makeVariableOutputTest, testLabel)
import Test.TestHarness.StreamCommon (makeBackpressureSignal)
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
  Signal System Bool ->
  Signal System (Bool, AXI4Stream 12)

data SampleNTTParams = SampleNTTParams
  { spReference :: ByteString -> [Word16],
    spTopEntity :: SampleNTTTopEntity
  }

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

runSampleNTTTest :: SampleNTTParams -> ShakeTest -> Expectation
runSampleNTTTest params test = do
  let expected = P.map reverseBits12Word (spReference params (testMessage test))
      actual = runSampleNTTHardware params test
  P.length actual `shouldBe` 256
  P.length expected `shouldBe` 256
  actual `shouldBe` expected

runSampleNTTHardware :: SampleNTTParams -> ShakeTest -> [Word16]
runSampleNTTHardware params test =
  let msgBV = bsToBV272 (testMessage test)
      msgDataSig = pure msgBV
      -- MSG_TVALID is True from the start (can be delayed with upstream stall pattern)
      msgValidSig = makeUpstreamValidSignal (testUpstreamStall test)
      treadySignal = makeBackpressureSignal (testDownstreamBackpressure test)
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          msgDataSig
          msgValidSig
          treadySignal
      outputsPerBlock = 112
      squeezesNeeded = (256 P.+ outputsPerBlock - 1) `P.div` outputsPerBlock
      -- Add extra cycles for upstream stalls
      stallCycles = case testUpstreamStall test of
        NoUpstreamStall -> 0
        UpstreamStall pattern -> P.length (P.takeWhile P.id pattern)
      sampleCount =
        stallCycles
          P.+ 1 -- Idle -> Absorb transition
          P.+ 1 -- Absorb -> Permute transition
          P.+ 24
          P.+ squeezesNeeded P.* (outputsPerBlock P.+ 24)
          P.+ 200
      samples = sampleN @System sampleCount (bundle (output, treadySignal))
      validOutputs =
        [ tdata stream
          | ((_, stream), ready) <- samples,
            tvalid stream P.&& ready
        ]
      coeffs = P.map (P.fromIntegral . (unpack :: BitVector 12 -> Unsigned 12)) (P.take 256 validOutputs)
   in coeffs

-- | Generate MSG_TVALID signal based on upstream stall pattern
-- NoUpstreamStall: MSG_TVALID is True from the start
-- UpstreamStall pattern: MSG_TVALID is False during stall (True in pattern), then True forever
makeUpstreamValidSignal :: UpstreamStall -> Signal System Bool
makeUpstreamValidSignal NoUpstreamStall = pure True
makeUpstreamValidSignal (UpstreamStall pattern) =
  -- Pattern: True means stall (don't send valid), False means ready to send
  -- After pattern is exhausted, default to True (always valid)
  fromList (P.map P.not pattern P.++ P.repeat True)

bsToBV272 :: ByteString -> BitVector 272
bsToBV272 bs =
  let padded = BS.take 34 (bs P.<> BS.replicate 34 0)
      bytes = BS.unpack padded
      -- Bit-reverse each byte to match the Reversed permutation's expectations
      step acc w = (acc `shiftL` 8) .|. resize (reverseBits8 (pack (fromIntegral w :: BitVector 8)))
   in P.foldl step (0 :: BitVector 272) bytes

reverseBits8 :: BitVector 8 -> BitVector 8
reverseBits8 bv = pack (reverse (unpack bv :: Vec 8 Bit))

reverseBits12Word :: Word16 -> Word16
reverseBits12Word w =
  let bv = pack (fromIntegral w :: Unsigned 12)
      rev = pack (reverse (unpack bv :: Vec 12 Bit))
   in P.fromIntegral (unpack rev :: Unsigned 12)

--------------------------------------------------------------------------------
-- Unpacking Python 384-byte format
--------------------------------------------------------------------------------

-- | Unpack Python's 384-byte format to 256 coefficients
-- Python packs two 12-bit coefficients into 3 bytes (128 triplets):
--   c0 = byte0 + 256 * (byte1 & 0x0F)  (bits 0-11)
--   c1 = (byte1 >> 4) + 16 * byte2      (bits 12-23)
unpackPython384Bytes :: ByteString -> [Word16]
unpackPython384Bytes bs = go (BS.unpack bs)
  where
    go (b0 : b1 : b2 : rest) =
      let c0 = P.fromIntegral b0 P.+ 256 P.* (P.fromIntegral b1 .&. 0x0F)
          c1 = (P.fromIntegral b1 `shiftR` 4) P.+ 16 P.* P.fromIntegral b2
       in c0 : c1 : go rest
    go [] = []
    go _ = P.error "unpackPython384Bytes: Expected 384 bytes (multiple of 3)"
