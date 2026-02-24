{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.SampleNTT512 (spec) where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude (BitVector, bundle, clockGen, enableGen, fromList, resetGen, sampleN, (++#))
import Component.SampleNTT512 qualified as SampleNTT512
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Maybe (isJust)
import Stream
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (forAll, withMaxSuccess)
import Test.TestHarness.SampleNTT.Common
  ( ShakeTest (..),
    UpstreamStall (..),
    backpressurePattern,
    bsToBV272Normal,
    externalSampleNTTPacked,
    getTimingInfo,
    testLabel,
    unpackPython384Bytes,
  )
import Test.TestHarness.SampleNTTSamples qualified as Samples
import Prelude (Maybe (..), ($))
import Prelude qualified as P

spec :: Spec
spec = describe "SampleNTT512 Stream" $ do
  describe "i272o24" $ runAllTests SampleNTT512.i272o24
  describe "i272o24l1" $ runAllTests (SampleNTT512.i272o24l1 SampleNTT512.Lookahead1)
  where
    runAllTests topEntityCore = do
      describe "Basic functionality tests (34-byte seeds)" $
        for_ Samples.basicSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTest topEntityCore testCase
      describe "Upstream stall handling (34-byte seeds)" $
        for_ Samples.stallSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTest topEntityCore testCase
      describe "Downstream backpressure handling (34-byte seeds)" $
        for_ Samples.backpressureSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTest topEntityCore testCase
      describe "Combined stress tests (34-byte seeds)" $
        for_ Samples.combinedSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTest topEntityCore testCase
      describe "QuickCheck property tests (34-byte seeds)" $
        it "correctly handles random 34-byte test cases" $
          withMaxSuccess 40 $ forAll Samples.genSampleNTTTest (runStreamTest topEntityCore)

    runStreamTest topEntityCore testCase =
      let seed = testMessage testCase
          holdCycles =
            case testUpstreamStall testCase of
              NoUpstreamStall -> 0
              UpstreamStall pattern -> P.length (P.takeWhile P.id pattern)
          inputTiming =
            if holdCycles P.== 0
              then [Input [bsToBV272Normal seed]]
              else [Hold holdCycles, Input [bsToBV272Normal seed]]
          bpPattern = backpressurePattern (testDownstreamBackpressure testCase)
          backpressureTiming =
            [ if b then Ready (P.length grp) else Backpress (P.length grp)
              | grp@(b : _) <- L.group bpPattern
            ]
          topEntity clk rst en treadySig inputSig =
            topEntityCore clk rst en (bundle (P.fmap P.fst inputSig, treadySig))
          expected = simulate seed backpressureTiming inputTiming
       in runStreamInputExpected topEntity expected inputTiming backpressureTiming

simulate :: ByteString -> BackpressureTiming -> InputTiming 272 -> OutputTiming 24
simulate seed backpressureTiming inputTiming =
  let (inputPattern, _) = expandInputTiming inputTiming
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "SampleNTT512.simulate: no input provided"
      validityRaw = getTimingInfo seed
      validity =
        if P.odd (P.length validityRaw)
          then validityRaw P.++ [P.False]
          else validityRaw
      coeffs = unpackPython384Bytes (externalSampleNTTPacked seed)
      pairs = toPairs coeffs
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> P.repeat P.True
        _ -> P.cycle readyPattern
      blocks = buildBlocks validity (0 :: P.Int) pairs (0 :: P.Int)
      (idleOut, readyAfterIdle) = consumeN startSilence readyStream
      (permuteOut, readyAfterPermute) = consumeN 25 readyAfterIdle
      (squeezeOut, _) = runBlocks blocks readyAfterPermute
      base = compress (idleOut P.++ permuteOut P.++ squeezeOut)
   in base
  where
    toPairs (a : b : rest) = (toBV12 b ++# toBV12 a) : toPairs rest
    toPairs _ = []

    toBV12 v = P.fromIntegral v :: BitVector 12

    buildBlocks validity buffer pairs emitted =
      if emitted P.>= 128
        then []
        else
          let (blockOuts, buffer', pairs', emitted', restVals) =
                runBlock validity 56 buffer pairs emitted []
           in if emitted' P.>= 128
                then [blockOuts]
                else blockOuts : buildBlocks restVals buffer' pairs' emitted'

    runBlock ::
      [P.Bool] ->
      P.Int ->
      P.Int ->
      [BitVector 24] ->
      P.Int ->
      [Maybe (BitVector 24)] ->
      ([Maybe (BitVector 24)], P.Int, [BitVector 24], P.Int, [P.Bool])
    runBlock vals cyclesLeft buffer pairs emitted acc =
      if emitted P.>= 128
        then (P.reverse acc, buffer, pairs, emitted, vals)
      else
        if cyclesLeft P.== 0
          then (P.reverse acc, buffer, pairs, emitted, vals)
          else
            case vals of
              v0 : v1 : vs ->
                let (buffer', emittedThis) = consume buffer v0 v1
                    (out, pairs', emitted') =
                      if emittedThis
                        then case pairs of
                          p : ps -> (Just p, ps, emitted P.+ 1)
                          [] -> P.error "SampleNTT512.simulate: output exhausted"
                        else (Nothing, pairs, emitted)
                 in runBlock vs (cyclesLeft P.- 1) buffer' pairs' emitted' (out : acc)
              [v0] ->
                let (buffer', emittedThis) = consume buffer v0 P.False
                    (out, pairs', emitted') =
                      if emittedThis
                        then case pairs of
                          p : ps -> (Just p, ps, emitted P.+ 1)
                          [] -> P.error "SampleNTT512.simulate: output exhausted"
                        else (Nothing, pairs, emitted)
                 in runBlock [] (cyclesLeft P.- 1) buffer' pairs' emitted' (out : acc)
              [] -> P.error "SampleNTT512.simulate: validity pattern exhausted"

    runBlocks [] rs = ([], rs)
    runBlocks (block : rest) rs =
      let (squeezeOut, rs') = runSqueeze block rs
          (permuteOut, rs'') =
            if P.null rest
              then ([], rs')
              else consumeN 24 rs'
          (moreOut, rs''') = runBlocks rest rs''
       in (squeezeOut P.++ permuteOut P.++ moreOut, rs''')

    runSqueeze [] rs = ([], rs)
    runSqueeze (b : bs) rs =
      case rs of
        r : rs' ->
          if r
            then
              let (out, rs'') = runSqueeze bs rs'
               in (b : out, rs'')
            else
              let (out, rs'') = runSqueeze (b : bs) rs'
               in (Nothing : out, rs'')
        [] -> P.error "SampleNTT512.simulate: empty backpressure pattern"

    consume buffer v0 v1 =
      let step (b, emitted) v =
            let b' = if v then b P.+ 1 else b
             in if P.not emitted P.&& b' P.>= 2
                  then (b' P.- 2, P.True)
                  else (b', emitted)
          (b1, e1) = step (buffer, P.False) v0
       in step (b1, e1) v1

    consumeN :: P.Int -> [P.Bool] -> ([Maybe (BitVector 24)], [P.Bool])
    consumeN n rs =
      case n of
        0 -> ([], rs)
        _ ->
          case rs of
            _ : rs' ->
              let (out, rs'') = consumeN (n P.- 1) rs'
               in (Nothing : out, rs'')
            [] -> P.error "SampleNTT512.simulate: empty backpressure pattern"

    compress [] = []
    compress xs =
      case P.span (\v -> P.not (isJust v)) xs of
        (nothings, rest) | P.not (P.null nothings) ->
          Silent (P.length nothings) : compress rest
        _ ->
          let (justs, rest) = P.span isJust xs
              vals = [v | Just v <- justs]
           in Output vals : compress rest

runStreamInputExpected ::
  StreamTopEntityIn 24 272 ->
  OutputTiming 24 ->
  InputTiming 272 ->
  BackpressureTiming ->
  P.IO ()
runStreamInputExpected topEntity expected inputTiming backpressureTiming = do
  let expectedBase = expandOutputTiming expected
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> P.repeat P.True
        _ -> P.cycle readyPattern
      (inputPattern, _inputValues) = expandInputTiming inputTiming
      lastJustIdx = case [i | (i, Just _) <- P.zip ([0 ..] :: [P.Int]) inputPattern] of
        [] -> Nothing
        xs -> Just (P.last xs)
      beats = P.zipWith (mkBeat lastJustIdx) ([0 ..] :: [P.Int]) inputPattern
      inputSignal = fromList (idleBeat : beats P.++ P.repeat idleBeat)
      treadySignal = fromList (P.True : readyStream)
      output =
        topEntity
          clockGen
          resetGen
          enableGen
          treadySignal
          (bundle (inputSignal, P.pure P.False))
      sampleLen = P.max (P.length expectedBase) (P.length inputPattern) P.+ 1
      samples = sampleN sampleLen (bundle (output, treadySignal, inputSignal))
      actualAll =
        [ if tvalid stream P.&& ready then Just (tdata stream) else Nothing
          | ((stream, _), ready, _) <- samples
        ]
      actual = P.take (P.length expectedBase) (P.drop 1 actualAll)
      inputValids = [tvalid inStream | (_, _, inStream) <- samples]
      readyPrev = P.True : [seedReady | ((_, seedReady), _, _) <- P.init samples]
      readyViolations = [() | (valid, ready) <- P.zip inputValids readyPrev, valid P.&& P.not ready]
      handshakes = [() | (valid, ready) <- P.zip inputValids readyPrev, valid P.&& ready]
  if P.null readyViolations
    then
      if P.null handshakes
        then P.error "SampleNTT512 Stream: MSG_TVALID never asserted after MSG_TREADY"
        else actual `shouldBe` expectedBase
    else P.error "SampleNTT512 Stream: MSG_TVALID asserted without MSG_TREADY in previous cycle"
  where
    mkBeat lastIdx i mv =
      AXI4Stream
        { tdata = case mv of
            Just v -> v
            Nothing -> 0,
          tvalid = isJust mv,
          tlast = case (lastIdx, mv) of
            (Just j, Just _) -> i P.== j
            _ -> P.False
        }
    idleBeat =
      AXI4Stream
        { tdata = 0,
          tvalid = P.False,
          tlast = P.False
        }
