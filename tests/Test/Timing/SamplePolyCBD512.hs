{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Timing.SamplePolyCBD512 (spec) where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.SamplePolyCBD512 qualified as SamplePolyCBD512
import Data.Bits qualified as Bits
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it, shouldBe)
import Timing
import Prelude qualified as P

spec :: Spec
spec = describe "Timing.SamplePolyCBD512" $ do
  it "matches expected handshake timing (no backpressure)" $ do
    runTimingTest
      [Silent 25 "absorb+permute", Output 181, Silent 25 "permute", Output 75]
      [Input 1, Hold 305]
      [Ready 306]

  it "matches expected handshake timing (periodic backpressure)" $ do
    runTimingTest
      [Silent 25 "absorb+permute", Output 15, Silent 10 "backpress", Output 166, Silent 25 "permute", Output 75]
      [Input 1, Hold 315]
      [Ready 40, Backpress 10, Ready 266]

  it "matches expected handshake timing (initial backpressure)" $ do
    runTimingTest
      [Silent 25 "absorb+permute", Output 181, Silent 25 "permute", Output 75]
      [Input 1, Hold 305]
      [Backpress 12, Ready 294]

runTimingTest :: OutputTiming -> InputTiming -> BackpressureTiming -> IO ()
runTimingTest outputTiming inputTiming backpressureTiming = do
  let seed = BS.pack [0 .. 31]
      b = 0x01
      msgBV = bsToBV264Normal (seed P.<> BS.singleton b)
      expectedBase = expandOutputTiming outputTiming
      inputPattern = expandInputTiming inputTiming
      treadyPattern = expandBackpressureTiming backpressureTiming
      sampleLen = P.length expectedBase
      expected = expectedBase
      treadySignal = fromList (treadyPattern P.++ P.repeat True)
      msgSig = pure msgBV
      output =
        SamplePolyCBD512.i264o12
          clockGen
          resetGen
          enableGen
          msgSig
          treadySignal
      samples = sampleN @System sampleLen (bundle (output, treadySignal))
      actual =
        [ tvalid stream P.&& ready
          | ((stream, _), ready) <- samples
        ]
  if P.length inputPattern /= sampleLen
    then P.error "Timing: InputTiming length must match OutputTiming length"
    else
      if P.length treadyPattern /= sampleLen
        then P.error "Timing: BackpressureTiming length must match OutputTiming length"
        else actual `shouldBe` expected

bsToBV264Normal :: BS.ByteString -> BitVector 264
bsToBV264Normal bs =
  let padded = BS.take 33 (bs P.<> BS.replicate 33 0)
      bits = P.concatMap word8ToBits (BS.unpack padded)
      paddedBits = P.take 264 (bits P.++ P.repeat 0)
   in P.foldl accumBit 0 (P.zip [0 .. 263] paddedBits)
  where
    word8ToBits :: Word8 -> [Bit]
    word8ToBits w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 7]]
    accumBit acc (i, b) = if b == 1 then Bits.setBit acc i else acc
