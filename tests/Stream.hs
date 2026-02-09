{-# LANGUAGE TypeApplications #-}

module Stream
  ( Output (..),
    OutputTiming,
    expandOutputTiming,
    Input (..),
    InputTiming,
    expandInputTiming,
    Backpressure (..),
    BackpressureTiming,
    expandBackpressureTiming,
    StreamTopEntity,
    run,
    toBV,
    bsToBVRev8,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude (Bit, BitVector, Clock, Enable, KnownNat, Reset, Signal, System, Vec, bundle, clockGen, enableGen, fromList, natToNum, pack, resize, resetGen, sampleN, shiftL, unpack)
import Clash.Prelude qualified as C
import Data.Bits (setBit, testBit, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (catMaybes)
import Data.Word (Word8)
import Test.Hspec (shouldBe)
import Prelude

--------------------------------------------------------------------------------

data Output n
  = Silent Int -- cycles of silence
  | Output [BitVector n] -- expected output data on handshake cycles
  deriving (Eq)

type OutputTiming n = [Output n]

--------------------------------------------------------------------------------

data Input n
  = Hold Int -- cycles with no input, i.e. when input tvalid is low
  | Input [BitVector n] -- input data when input tvalid is high

type InputTiming n = [Input n]

expandInputTiming :: InputTiming n -> [Maybe (BitVector n)]
expandInputTiming = concatMap expand
  where
    expand :: Input n -> [Maybe (BitVector n)]
    expand (Hold n) = replicate n Nothing
    expand (Input xs) = map Just xs

data Backpressure
  = Ready Int -- cycles when output tready is high (ready to accept output)
  | Backpress Int -- cycles when output tready is low (backpressure applied)

type BackpressureTiming = [Backpressure]

expandBackpressureTiming :: BackpressureTiming -> [Bool]
expandBackpressureTiming = concatMap expand
  where
    expand :: Backpressure -> [Bool]
    expand (Ready n) = replicate n True
    expand (Backpress n) = replicate n False

expandOutputTiming :: OutputTiming n -> [Maybe (BitVector n)]
expandOutputTiming = concatMap expand
  where
    expand :: Output n -> [Maybe (BitVector n)]
    expand (Silent n) = replicate n Nothing
    expand (Output xs) = map Just xs

type StreamTopEntity m n =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector n) ->
  Signal System Bool ->
  Signal System (AXI4Stream m, Bool)

run :: (KnownNat n, KnownNat m) => StreamTopEntity m n -> OutputTiming m -> InputTiming n -> BackpressureTiming -> IO ()
run topEntity outputTiming inputTiming backpressureTiming = do
  let expectedBase = expandOutputTiming outputTiming
      inputPattern = expandInputTiming inputTiming
      treadyPattern = expandBackpressureTiming backpressureTiming
      sampleLen = length expectedBase
      input = case catMaybes inputPattern of
        (v : _) -> v
        [] -> 0
      treadySignal = fromList (True : treadyPattern ++ repeat True)
      msgSig = pure input
      output =
        topEntity
          clockGen
          resetGen
          enableGen
          msgSig
          treadySignal
      samples = sampleN @System (sampleLen + 1) (bundle (output, treadySignal))
      actualAll =
        [ if tvalid stream && ready then Just (tdata stream) else Nothing
          | ((stream, _), ready) <- samples
        ]
      actual = drop 1 actualAll
  if length inputPattern /= sampleLen
    then error "Stream: InputTiming length must match OutputTiming length"
    else
      if length treadyPattern /= sampleLen
        then error "Stream: BackpressureTiming length must match OutputTiming length"
        else actual `shouldBe` expectedBase

toBV :: forall n. (KnownNat n) => ByteString -> BitVector n
toBV bs =
  let bits = concatMap word8ToBits (BS.unpack bs)
      paddedBits = take (natToNum @n) (bits ++ repeat 0)
   in foldl accumBit 0 (zip [0 ..] paddedBits)
  where
    word8ToBits :: Word8 -> [Bit]
    word8ToBits w = [if testBit w i then 1 else 0 | i <- [0 .. 7]]
    accumBit acc (i, b) = if b == 1 then setBit acc i else acc

bsToBVRev8 :: forall n. (KnownNat n) => ByteString -> BitVector n
bsToBVRev8 bs =
  let byteCount = (natToNum @n + 7) `div` 8
      padded = BS.take byteCount (bs <> BS.replicate byteCount 0)
      step acc w =
        (acc `shiftL` 8)
          .|. resize (reverseBits8 (pack (fromIntegral w :: BitVector 8)))
   in foldl step 0 (BS.unpack padded)
  where
    reverseBits8 :: BitVector 8 -> BitVector 8
    reverseBits8 bv = pack (C.reverse (unpack bv :: Vec 8 Bit))
