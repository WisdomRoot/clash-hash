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
import Clash.Sized.Vector qualified as V
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word16, Word8)
import Test.Hspec (Expectation, shouldBe)
import Test.TestHarness.SHAKECommon (DownstreamBackpressure (..), ShakeTest (..), UpstreamStall (..), makeBackpressureTest, makeBasicTest, makeCombinedTest, makeStallTest, makeVariableOutputTest, testLabel)
import Test.TestHarness.StreamCommon
  ( makeBackpressureSignal,
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
  P.length actual `shouldBe` 256
  P.length expected `shouldBe` 256
  actual `shouldBe` expected

runSampleNTTHardware :: SampleNTTParams -> ShakeTest -> [Word16]
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
      outputWords = (256 P.+ 4) `P.div` 5
      squeezesNeeded = (outputWords P.+ beatsPerBlock - 1) `P.div` beatsPerBlock
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
      coeffs = P.concatMap wordToCoeffs (P.take outputWords validOutputs)
   in P.take 256 coeffs

bsToBV272 :: ByteString -> BitVector 272
bsToBV272 bs =
  let bytes = BS.unpack bs
      padded = P.take 34 (bytes P.++ P.repeat 0)
      vec :: Vec 34 (BitVector 8)
      vec = V.unsafeFromList (P.map (fromIntegral :: Word8 -> BitVector 8) padded)
   in pack vec

wordToCoeffs :: BitVector 64 -> [Word16]
wordToCoeffs w =
  let rawBytes = bytesFromWord w
      bytes = map bitReverse8 rawBytes
      c0 = coeffFromBytes bytes 0
      c1 = coeffFromBytes bytes 1
      c2 = coeffFromBytes bytes 2
      c3 = coeffFromBytes bytes 3
      c4 = coeffFromBytes bytes 4
   in P.map (P.fromIntegral :: Unsigned 12 -> Word16) [c0, c1, c2, c3, c4]

toU12 :: BitVector 8 -> Unsigned 12
toU12 b = resize (unpack b :: Unsigned 8)

bytesFromWord :: BitVector 64 -> Vec 8 (BitVector 8)
bytesFromWord w = map (byteFromWord w) indicesI

bitReverse8 :: BitVector 8 -> BitVector 8
bitReverse8 b = pack (reverse (unpack b :: Vec 8 Bit))

byteFromWord :: BitVector 64 -> Index 8 -> BitVector 8
byteFromWord w k =
  let base = fromIntegral k * 8
      bits :: Vec 8 Bit
      bits = map (\i -> boolToBit (testBit w (63 - (base + fromIntegral i)))) indicesI
   in pack bits

coeffFromBytes :: Vec 8 (BitVector 8) -> Index 16 -> Unsigned 12
coeffFromBytes bytes pointer =
  let i0 = 0 :: Index 8
      i1 = 1 :: Index 8
      i2 = 2 :: Index 8
      i3 = 3 :: Index 8
      i4 = 4 :: Index 8
      i5 = 5 :: Index 8
      i6 = 6 :: Index 8
      i7 = 7 :: Index 8
      b0 = bytes !! i0
      b1 = bytes !! i1
      b2 = bytes !! i2
      b3 = bytes !! i3
      b4 = bytes !! i4
      b5 = bytes !! i5
      b6 = bytes !! i6
      b7 = bytes !! i7
      d1' x y = toU12 x + shiftL (resize (unpack (y .&. 0x0F) :: Unsigned 8)) 8
      d2' x y = resize (unpack (x `shiftR` 4) :: Unsigned 8) + shiftL (toU12 y) 4
   in case pointer of
        0 -> d1' b0 b1
        1 -> d2' b1 b2
        2 -> d1' b3 b4
        3 -> d2' b4 b5
        _ -> d1' b6 b7

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
