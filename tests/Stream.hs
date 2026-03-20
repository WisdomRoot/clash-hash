{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use catMaybes" #-}

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
    genBackpressure,
    StreamTopEntity,
    run,
    StreamTopEntityIn,
    runStreamInput,
    runPipeInput,
    runPipeInputExact,
    runPipe2Input,
    runPipe2InputExact,
    runPipeCtrlInput,
    runPipeCtrl2Input,
    toBV,
    bvToBS,
    genInputBV
  )
where

import AXI4Stream (AXI4Stream (..), Pipe, Pipe2, PipeCtrl, PipeCtrl2, runPipe2, runPipeCtrl2)
import Clash.Prelude (Bit, BitVector, Clock, Enable, KnownNat, NFDataX, Reset, Signal, System, bundle, clockGen, enableGen, fromList, natToNum, register, resetGen, sampleN, withClockResetEnable)
import Data.Bits (setBit, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.Hspec (shouldBe)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, shuffle, vectorOf)
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

compressBackpressure :: [Bool] -> BackpressureTiming
compressBackpressure [] = []
compressBackpressure (b : bs) =
  let (same, rest) = span (== b) bs
      len = 1 + length same
      tag = if b then Ready len else Backpress len
   in tag : compressBackpressure rest

genBackpressure :: Gen BackpressureTiming
genBackpressure = do
  bools <-
    shuffle
      ( replicate 8 True
          ++ replicate 2 False
      )
  pure (compressBackpressure bools)

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
applyBackpressureUntil 0 _ _ = []
applyBackpressureUntil _ [] _ = error "Stream.run: expected output shorter than handshake count"
applyBackpressureUntil n bs [] = applyBackpressureUntil n bs (repeat True)
applyBackpressureUntil n (b : bs) (r : rs) =
      case b of
        Nothing -> Nothing : applyBackpressureUntil n bs rs
        Just _ ->
          if r
            then b : applyBackpressureUntil (n - 1) bs rs
            else Nothing : applyBackpressureUntil n (b : bs) rs

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
      flushSignal = pure False
      treadySignal = fromList (True : readyStream)
      output =
        let inputSignal = driveInput inputPattern inReady
            out =
              topEntity
                clockGen
                resetGen
                enableGen
                treadySignal
                (bundle (inputSignal, flushSignal))
            inReady = fmap snd out
         in out
      samples = sampleN @System (length expectedBase + 1) (bundle (output, treadySignal))
      actualAll =
        [ if tvalid stream && ready then Just (tdata stream) else Nothing
          | ((stream, _), ready) <- samples
        ]
      actual = drop 1 actualAll
  actual `shouldBe` expectedBase

runPipeInput ::
  (KnownNat n, KnownNat m) =>
  Pipe System n m ->
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
      treadySignal = fromList (True : readyStream)
      output =
        let inputSignal = driveInput inputPattern inReady
            (inReady, outStream) = pipeEntity (treadySignal, inputSignal)
         in bundle (outStream, inReady)
      samples = sampleN @System (length expectedBase + 1) (bundle (output, treadySignal))
      actualAll =
        [ if tvalid stream && ready then Just (tdata stream) else Nothing
          | ((stream, _), ready) <- samples
        ]
      actual = drop 1 actualAll
  actual `shouldBe` expectedBase

runPipe2Input ::
  (KnownNat n, KnownNat m) =>
  Pipe2 System n m ->
  (InputTiming n -> OutputTiming m) ->
  InputTiming n ->
  BackpressureTiming ->
  IO ()
runPipe2Input pipeEntity simulate inputTiming backpressureTiming = do
  let base = expandOutputTiming (simulate inputTiming)
      expectedHandshakes = length [() | Just _ <- base]
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> repeat True
        _ -> cycle readyPattern
      expectedBase = applyBackpressureUntil expectedHandshakes base readyStream
      (inputPattern, _inputValues) = expandInputTiming inputTiming
      treadySignal = fromList (True : readyStream)
      output =
        withClockResetEnable clockGen resetGen enableGen $
          let inputSignal = driveInput inputPattern inReady
              (inReady, outStream) = runPipe2 pipeEntity (treadySignal, inputSignal)
           in bundle (outStream, inReady)
      samples = sampleN @System (length expectedBase + 1) (bundle (output, treadySignal))
      actualAll =
        [ if tvalid stream && ready then Just (tdata stream) else Nothing
          | ((stream, _), ready) <- samples
        ]
      actual = drop 1 actualAll
  actual `shouldBe` expectedBase

runPipeInputExact ::
  (KnownNat n, KnownNat m) =>
  Pipe System n m ->
  (InputTiming n -> BackpressureTiming -> OutputTiming m) ->
  InputTiming n ->
  BackpressureTiming ->
  IO ()
runPipeInputExact pipeEntity simulate inputTiming backpressureTiming = do
  let expectedBase = expandOutputTiming (simulate inputTiming backpressureTiming)
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> repeat True
        _ -> cycle readyPattern
      (inputPattern, _inputValues) = expandInputTiming inputTiming
      treadySignal = fromList (True : readyStream)
      output =
        let inputSignal = driveInput inputPattern inReady
            (inReady, outStream) = pipeEntity (treadySignal, inputSignal)
         in bundle (outStream, inReady)
      samples = sampleN @System (length expectedBase + 1) (bundle (output, treadySignal))
      actualAll =
        [ if tvalid stream && ready then Just (tdata stream) else Nothing
          | ((stream, _), ready) <- samples
        ]
      actual = drop 1 actualAll
  actual `shouldBe` expectedBase

runPipe2InputExact ::
  (KnownNat n, KnownNat m) =>
  Pipe2 System n m ->
  (InputTiming n -> BackpressureTiming -> OutputTiming m) ->
  InputTiming n ->
  BackpressureTiming ->
  IO ()
runPipe2InputExact pipeEntity simulate inputTiming backpressureTiming = do
  let expectedBase = expandOutputTiming (simulate inputTiming backpressureTiming)
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> repeat True
        _ -> cycle readyPattern
      (inputPattern, _inputValues) = expandInputTiming inputTiming
      treadySignal = fromList (True : readyStream)
      output =
        withClockResetEnable clockGen resetGen enableGen $
          let inputSignal = driveInput inputPattern inReady
              (inReady, outStream) = runPipe2 pipeEntity (treadySignal, inputSignal)
           in bundle (outStream, inReady)
      samples = sampleN @System (length expectedBase + 1) (bundle (output, treadySignal))
      actualAll =
        [ if tvalid stream && ready then Just (tdata stream) else Nothing
          | ((stream, _), ready) <- samples
        ]
      actual = drop 1 actualAll
  actual `shouldBe` expectedBase

runPipeCtrlInput ::
  (KnownNat n, KnownNat m, NFDataX c) =>
  PipeCtrl System c n m ->
  c ->
  [c] ->
  ([c] -> InputTiming n -> OutputTiming m) ->
  InputTiming n ->
  BackpressureTiming ->
  IO ()
runPipeCtrlInput pipeEntity ctrlDefault ctrlPattern simulate inputTiming backpressureTiming = do
  let base = expandOutputTiming (simulate ctrlPattern inputTiming)
      expectedHandshakes = length [() | Just _ <- base]
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> repeat True
        _ -> cycle readyPattern
      expectedBase = applyBackpressureUntil expectedHandshakes base readyStream
      (inputPattern, _inputValues) = expandInputTiming inputTiming
      ctrlSignal = fromList (ctrlDefault : ctrlPattern ++ repeat ctrlDefault)
      treadySignal = fromList (True : readyStream)
      output =
        let inputSignal = driveInput inputPattern inReady
            (inReady, outStream) = pipeEntity (treadySignal, ctrlSignal, inputSignal)
         in bundle (outStream, inReady)
      samples = sampleN @System (length expectedBase + 1) (bundle (output, treadySignal))
      actualAll =
        [ if tvalid stream && ready then Just (tdata stream) else Nothing
          | ((stream, _), ready) <- samples
        ]
      actual = drop 1 actualAll
  actual `shouldBe` expectedBase

runPipeCtrl2Input ::
  (KnownNat n, KnownNat m, NFDataX c) =>
  PipeCtrl2 System c n m ->
  c ->
  [c] ->
  ([c] -> InputTiming n -> OutputTiming m) ->
  InputTiming n ->
  BackpressureTiming ->
  IO ()
runPipeCtrl2Input pipeEntity ctrlDefault ctrlPattern simulate inputTiming backpressureTiming = do
  let base = expandOutputTiming (simulate ctrlPattern inputTiming)
      expectedHandshakes = length [() | Just _ <- base]
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> repeat True
        _ -> cycle readyPattern
      expectedBase = applyBackpressureUntil expectedHandshakes base readyStream
      (inputPattern, _inputValues) = expandInputTiming inputTiming
      ctrlSignal = fromList (ctrlDefault : ctrlPattern ++ repeat ctrlDefault)
      treadySignal = fromList (True : readyStream)
      output =
        withClockResetEnable clockGen resetGen enableGen $
          let inputSignal = driveInput inputPattern inReady
              (inReady, outStream) = runPipeCtrl2 pipeEntity (treadySignal, ctrlSignal, inputSignal)
           in bundle (outStream, inReady)
      samples = sampleN @System (length expectedBase + 1) (bundle (output, treadySignal))
      actualAll =
        [ if tvalid stream && ready then Just (tdata stream) else Nothing
          | ((stream, _), ready) <- samples
        ]
      actual = drop 1 actualAll
  actual `shouldBe` expectedBase

driveInput ::
  KnownNat n =>
  [Maybe (BitVector n)] ->
  Signal System Bool ->
  Signal System (AXI4Stream n)
driveInput inputPattern readySig =
  withClockResetEnable clockGen resetGen enableGen $
    let state = register annotated (advance <$> state <*> readySig)
     in render <$> state
  where
    lastJustIdx =
      case [i | (i, Just _) <- zip ([0 ..] :: [Int]) inputPattern] of
        [] -> Nothing
        xs -> Just (last xs)
    annotated =
      zipWith
        (\i mv -> (\v -> (v, Just i == lastJustIdx)) <$> mv)
        ([0 ..] :: [Int])
        inputPattern

    advance [] _ = []
    advance (Nothing : rest) _ = rest
    advance pending@(Just _ : rest) ready =
      if ready then rest else pending

    render [] = idleBeat
    render (Nothing : _) = idleBeat
    render (Just (v, isLast) : _) =
      AXI4Stream
        { tdata = v,
          tvalid = True,
          tlast = isLast
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

bvToBS :: forall n. (KnownNat n) => Int -> BitVector n -> ByteString
bvToBS byteCount bv = BS.pack [byteAt i | i <- [0 .. byteCount - 1]]
  where
    byteAt :: Int -> Word8
    byteAt byteIdx =
      let base = byteIdx * 8
       in foldl
            (\acc bitIdx -> if testBit bv (base + bitIdx) then setBit acc bitIdx else acc)
            (0 :: Word8)
            [0 .. 7]

genInputBV :: forall n. (KnownNat n) => Int -> Gen (BitVector n)
genInputBV byteCount = do
  bytes <- vectorOf byteCount arbitrary
  pure (toBV @n (BS.pack bytes))
