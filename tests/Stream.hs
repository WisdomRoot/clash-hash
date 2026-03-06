{-# LANGUAGE RankNTypes #-}
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
    applyBackpressure,
    StreamTopEntity,
    run,
    StreamTopEntityIn,
    runStreamInput,
    runPipeInput,
    toBV,
    bsToBVRev8,
  )
where

import AXI4Stream (AXI4Stream (..), Pipe)
import Clash.Prelude (Bit, BitVector, Clock, Enable, HiddenClockResetEnable, KnownNat, Reset, Signal, System, Vec, bundle, clockGen, enableGen, fromList, natToNum, pack, resize, resetGen, sampleN, shiftL, unpack, withClockResetEnable)
import Clash.Prelude qualified as C
import Data.Bits (setBit, testBit, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (isJust, isNothing)
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
  deriving (Eq, Show)

type InputTiming n = [Input n]

expandInputTiming :: InputTiming n -> ([Maybe (BitVector n)], [BitVector n])
expandInputTiming = go
  where
    go [] = ([], [])
    go (Hold n : rest) =
      let (pattern, values) = go rest
       in (replicate n Nothing ++ pattern, values)
    go (Input xs : rest) =
      let (pattern, values) = go rest
       in (map Just xs ++ pattern, xs ++ values)

data Backpressure
  = Ready Int -- cycles when output tready is high (ready to accept output)
  | Backpress Int -- cycles when output tready is low (backpressure applied)
  deriving (Eq, Show)

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

applyBackpressure :: BackpressureTiming -> OutputTiming o -> OutputTiming o
applyBackpressure backpressureTiming outputTiming =
  let ready = expandBackpressureTiming backpressureTiming
      base = expandOutputTiming outputTiming
      (out, rest) = go base ready
   in if any isJust rest
        then error "Stream.applyBackpressure: backpressure pattern too short"
        else compress out
  where
    go base [] = ([], base)
    go base (r : rs) =
      case base of
        [] ->
          let (out, rest) = go [] rs
           in (Nothing : out, rest)
        (b : bs) ->
          case b of
            Nothing ->
              let (out, rest) = go bs rs
               in (Nothing : out, rest)
            Just _ ->
              if r
                then
                  let (out, rest) = go bs rs
                   in (b : out, rest)
                else
                  let (out, rest) = go base rs
                   in (Nothing : out, rest)

    compress [] = []
    compress xs =
      case span isNothing xs of
        (nothings, rest) | not (null nothings) ->
          Silent (length nothings) : compress rest
        _ ->
          let (justs, rest) = span isJust xs
              vals = [v | Just v <- justs]
           in Output vals : compress rest

type StreamTopEntity m n =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector n) ->
  Signal System Bool ->
  Signal System (AXI4Stream m, Bool)

type StreamTopEntityIn m n =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream n, Bool) ->
  Signal System (AXI4Stream m, Bool)

run :: (KnownNat n, KnownNat m) => StreamTopEntity m n -> (InputTiming n -> OutputTiming m) -> InputTiming n -> BackpressureTiming -> IO ()
run topEntity simulate inputTiming backpressureTiming = do
  let base = expandOutputTiming (simulate inputTiming)
      expectedHandshakes = length [() | Just _ <- base]
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> repeat True
        _ -> cycle readyPattern
      expectedBase = applyBackpressureUntil expectedHandshakes base readyStream
      (_inputPattern, inputValues) = expandInputTiming inputTiming
      sampleLen = length expectedBase
      input = case inputValues of
        (v : _) -> v
        [] -> 0
      treadySignal = fromList (True : readyStream)
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
  actual `shouldBe` expectedBase

applyBackpressureUntil :: Int -> [Maybe (BitVector m)] -> [Bool] -> [Maybe (BitVector m)]
applyBackpressureUntil target = go target
  where
    go 0 _ _ = []
    go _ [] _ = error "Stream.run: expected output shorter than handshake count"
    go n bs [] = go n bs (repeat True)
    go n (b : bs) (r : rs) =
      case b of
        Nothing -> Nothing : go n bs rs
        Just _ ->
          if r
            then b : go (n - 1) bs rs
            else Nothing : go n (b : bs) rs

runStreamInput ::
  (KnownNat n, KnownNat m) =>
  StreamTopEntityIn m n ->
  (InputTiming n -> OutputTiming m) ->
  InputTiming n ->
  BackpressureTiming ->
  IO ()
runStreamInput topEntity simulate inputTiming backpressureTiming = do
  let base = expandOutputTiming (simulate inputTiming)
      expectedHandshakes = length [() | Just _ <- base]
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> repeat True
        _ -> cycle readyPattern
      expectedBase = applyBackpressureUntil expectedHandshakes base readyStream
      (inputPattern, _inputValues) = expandInputTiming inputTiming
      lastJustIdx = case [i | (i, Just _) <- zip ([0 ..] :: [Int]) inputPattern] of
        [] -> Nothing
        xs -> Just (last xs)
      beats = zipWith (mkBeat lastJustIdx) ([0 ..] :: [Int]) inputPattern
      inputSignal = fromList (idleBeat : beats ++ repeat idleBeat)
      flushSignal = pure False
      treadySignal = fromList (True : readyStream)
      output =
        topEntity
          clockGen
          resetGen
          enableGen
          treadySignal
          (bundle (inputSignal, flushSignal))
      samples = sampleN @System (length expectedBase + 1) (bundle (output, treadySignal))
      actualAll =
        [ if tvalid stream && ready then Just (tdata stream) else Nothing
          | ((stream, _), ready) <- samples
        ]
      actual = drop 1 actualAll
  actual `shouldBe` expectedBase
  where
    mkBeat lastIdx i mv =
      AXI4Stream
        { tdata = case mv of
            Just v -> v
            Nothing -> 0,
          tvalid = isJust mv,
          tlast = case (lastIdx, mv) of
            (Just j, Just _) -> i == j
            _ -> False
        }
    idleBeat =
      AXI4Stream
        { tdata = 0,
          tvalid = False,
          tlast = False
        }

runPipeInput ::
  (KnownNat n, KnownNat m) =>
  (forall dom. HiddenClockResetEnable dom => Pipe dom n m) ->
  (InputTiming n -> OutputTiming m) ->
  InputTiming n ->
  BackpressureTiming ->
  IO ()
runPipeInput pipeEntity simulate inputTiming backpressureTiming = do
  let base = expandOutputTiming (simulate inputTiming)
      expectedHandshakes = length [() | Just _ <- base]
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> repeat True
        _ -> cycle readyPattern
      expectedBase = applyBackpressureUntil expectedHandshakes base readyStream
      (inputPattern, _inputValues) = expandInputTiming inputTiming
      lastJustIdx = case [i | (i, Just _) <- zip ([0 ..] :: [Int]) inputPattern] of
        [] -> Nothing
        xs -> Just (last xs)
      beats = zipWith (mkBeat lastJustIdx) ([0 ..] :: [Int]) inputPattern
      inputSignal = fromList (idleBeat : beats ++ repeat idleBeat)
      treadySignal = fromList (True : readyStream)
      output =
        withClockResetEnable clockGen resetGen enableGen $
          let (inReady, outStream) = pipeEntity (treadySignal, inputSignal)
           in bundle (outStream, inReady)
      samples = sampleN @System (length expectedBase + 1) (bundle (output, treadySignal))
      actualAll =
        [ if tvalid stream && ready then Just (tdata stream) else Nothing
          | ((stream, _), ready) <- samples
        ]
      actual = drop 1 actualAll
  actual `shouldBe` expectedBase
  where
    mkBeat lastIdx i mv =
      AXI4Stream
        { tdata = case mv of
            Just v -> v
            Nothing -> 0,
          tvalid = isJust mv,
          tlast = case (lastIdx, mv) of
            (Just j, Just _) -> i == j
            _ -> False
        }
    idleBeat =
      AXI4Stream
        { tdata = 0,
          tvalid = False,
          tlast = False
        }

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
