{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Lookahead4 (spec) where

import AXI4Stream (Pipe2)
import Clash.Prelude (BitVector, SNat (..), System, slice, (++#))
import Component.SampleNTT4 qualified as SampleNTT4
import Data.Maybe (isJust, isNothing)
import Stream
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, chooseInt, forAll, vectorOf)
import Prelude

lookahead4AsPipe :: Pipe2 System 72 24
lookahead4AsPipe = SampleNTT4.lookahead4

packCandidates :: [Int] -> BitVector 72
packCandidates coeffs =
  case coeffs of
    [c0, c1, c2, c3, c4, c5] ->
      to12 c5 ++# to12 c4 ++# to12 c3 ++# to12 c2 ++# to12 c1 ++# to12 c0
    _ -> error "Test.Lookahead4.packCandidates: expected 6 candidates"
  where
    to12 :: Int -> BitVector 12
    to12 = fromIntegral

unpackCandidates :: BitVector 72 -> [BitVector 12]
unpackCandidates beat =
  [ slice (SNat @11) (SNat @0) beat,
    slice (SNat @23) (SNat @12) beat,
    slice (SNat @35) (SNat @24) beat,
    slice (SNat @47) (SNat @36) beat,
    slice (SNat @59) (SNat @48) beat,
    slice (SNat @71) (SNat @60) beat
  ]

validCoeffs :: BitVector 72 -> [BitVector 12]
validCoeffs beat = [c | c <- unpackCandidates beat, c < 3329]

popPair :: [BitVector 12] -> (BitVector 24, [BitVector 12])
popPair (a : b : rest) = (b ++# a, rest)
popPair _ = error "Test.Lookahead4.popPair: buffer underflow"

simulateExact :: InputTiming 72 -> BackpressureTiming -> OutputTiming 24
simulateExact inputTiming backpressureTiming =
  compressOutputs (go inputPattern [] readyStream)
  where
    (inputPattern, _inputValues) = expandInputTiming inputTiming
    readyPattern = expandBackpressureTiming backpressureTiming
    readyStream = case readyPattern of
      [] -> repeat True
      _ -> cycle readyPattern

    go ::
      [Maybe (BitVector 72)] ->
      [BitVector 12] ->
      [Bool] ->
      [Maybe (BitVector 24)]
    go pattern buffer (coeffReady : restReady) =
      let inputReady = length buffer < 2
          (currentBeat, nextPattern) = sourceStep inputReady pattern
          (nextBuffer, outBeat) = stageStep buffer currentBeat coeffReady
          handshakeBeat =
            case outBeat of
              Just pair | coeffReady -> Just pair
              _ -> Nothing
          done = null nextPattern && length nextBuffer < 2
       in if done
            then [handshakeBeat]
            else handshakeBeat : go nextPattern nextBuffer restReady
    go _ _ [] = error "Test.Lookahead4.simulateExact: impossible empty ready stream"

    sourceStep ::
      Bool ->
      [Maybe (BitVector 72)] ->
      (Maybe (BitVector 72), [Maybe (BitVector 72)])
    sourceStep _ [] = (Nothing, [])
    sourceStep _ (Nothing : rest) = (Nothing, rest)
    sourceStep True (beat : rest) = (beat, rest)
    sourceStep False pattern@(Just _ : _) = (head pattern, pattern)

    stageStep ::
      [BitVector 12] ->
      Maybe (BitVector 72) ->
      Bool ->
      ([BitVector 12], Maybe (BitVector 24))
    stageStep buffer currentBeat coeffReady
      | length buffer >= 2 =
          let (pair, rest) = popPair buffer
           in if coeffReady
                then (rest, Just pair)
                else (buffer, Just pair)
      | otherwise =
          case currentBeat of
            Nothing -> (buffer, Nothing)
            Just beat ->
              let queue = buffer ++ validCoeffs beat
               in if length queue >= 2 && coeffReady
                    then
                      let (pair, rest) = popPair queue
                       in (rest, Just pair)
                    else (queue, Nothing)

    compressOutputs :: [Maybe (BitVector 24)] -> OutputTiming 24
    compressOutputs [] = []
    compressOutputs xs =
      case span isNothing xs of
        (nothings, rest) | not (null nothings) ->
          Silent (length nothings) : compressOutputs rest
        _ ->
          let (justs, rest) = span isJust xs
              vals = [v | Just v <- justs]
           in Output vals : compressOutputs rest

genCase :: Gen (InputTiming 72, BackpressureTiming)
genCase = do
  beatCount <- chooseInt (1, 4)
  coeffss <- vectorOf beatCount (vectorOf 6 (chooseInt (0, 4095)))
  backpressure <- genBackpressure
  holdLen <- chooseInt (0, 5)
  let beats = map packCandidates coeffss
      inputTiming =
        if holdLen == 0
          then [Input beats]
          else [Hold holdLen, Input beats]
  pure (inputTiming, backpressure)

spec :: Spec
spec = describe "lookahead4" $ do
  it "drops invalid candidates and emits full pairs" $ do
    let beat = packCandidates [1, 4000, 2, 5000, 3, 6000]
    runPipe2InputExact lookahead4AsPipe simulateExact [Input [beat]] [Ready 1]
  it "drains buffered pairs before accepting the next input beat" $ do
    let beat0 = packCandidates [1, 2, 3, 4, 5, 6]
        beat1 = packCandidates [7, 8, 9, 10, 11, 12]
    runPipe2InputExact lookahead4AsPipe simulateExact [Input [beat0, beat1]] [Ready 1]
  it "propagates backpressure through the buffered drain path" $ do
    let beat0 = packCandidates [1, 2, 3, 4, 5, 6]
        beat1 = packCandidates [7, 8, 9, 10, 11, 12]
    runPipe2InputExact lookahead4AsPipe simulateExact [Input [beat0, beat1]] [Ready 2, Backpress 1]
  describe "QuickCheck property tests" $
    it "matches the exact screening model for random multi-beat inputs" $
      forAll genCase $ \(inputTiming, backpressure) ->
        runPipe2InputExact lookahead4AsPipe simulateExact inputTiming backpressure
