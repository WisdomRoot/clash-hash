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
  describe "i272o24l0" $ runAllTests SampleNTT512.Lookahead0 SampleNTT512.i272o24l0
  describe "i272o24l1" $ runAllTests SampleNTT512.Lookahead1 (SampleNTT512.i272o24l1 SampleNTT512.Lookahead1)
  where
    runAllTests lookahead topEntityCore = do
      describe "Basic functionality tests (34-byte seeds)" $
        for_ Samples.basicSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTest lookahead topEntityCore testCase
      describe "Upstream stall handling (34-byte seeds)" $
        for_ Samples.stallSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTest lookahead topEntityCore testCase
      describe "Downstream backpressure handling (34-byte seeds)" $
        for_ Samples.backpressureSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTest lookahead topEntityCore testCase
      describe "Combined stress tests (34-byte seeds)" $
        for_ Samples.combinedSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTest lookahead topEntityCore testCase
      describe "QuickCheck property tests (34-byte seeds)" $
        it "correctly handles random 34-byte test cases" $
          withMaxSuccess 40 $ forAll Samples.genSampleNTTTest (runStreamTest lookahead topEntityCore)

    runStreamTest lookahead topEntityCore testCase =
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
          expected = simulate lookahead seed backpressureTiming inputTiming
       in runStreamInputExpected topEntity expected inputTiming backpressureTiming

simulate :: SampleNTT512.Lookahead -> ByteString -> BackpressureTiming -> InputTiming 272 -> OutputTiming 24
simulate lookahead seed backpressureTiming inputTiming =
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

    lookN = case lookahead of
      SampleNTT512.Lookahead0 -> 2
      SampleNTT512.Lookahead1 -> 3

    decide0 buffer v0 v1 =
      case buffer of
        0 ->
          if v0 P.&& v1
            then (2, P.True, 0)
            else
              if v0 P.|| v1
                then (2, P.False, 1)
                else (2, P.False, 0)
        _ ->
          if v0 P.&& v1
            then (2, P.True, 1)
            else
              if v0 P.|| v1
                then (2, P.True, 0)
                else (2, P.False, 1)

    decide1 buffer v0 v1 v2 =
      case buffer of
        0 ->
          if v0 P.&& v1
            then (2, P.True, 0)
            else
              if v0 P.&& P.not v1 P.&& v2
                then (3, P.True, 0)
                else
                  if P.not v0 P.&& v1 P.&& v2
                    then (3, P.True, 0)
                    else
                      if v0 P.|| v1 P.|| v2
                        then (3, P.False, 1)
                        else (3, P.False, 0)
        _ ->
          if v0
            then (1, P.True, 0)
            else
              if v1
                then (2, P.True, 0)
                else
                  if v2
                    then (3, P.True, 0)
                    else (3, P.False, 1)

    buildBlocks validity buffer pairs emitted =
      if emitted P.>= 128
        then []
        else
          let (blockOuts, buffer', pairs', emitted', restVals) =
                runBlock validity 112 buffer pairs emitted []
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
    runBlock vals remaining buffer pairs emitted acc =
      if emitted P.>= 128
        then (P.reverse acc, buffer, pairs, emitted, vals)
      else
        if remaining P.== 0
          then (P.reverse acc, buffer, pairs, emitted, vals)
          else
            case vals of
              [] -> P.error "SampleNTT512.simulate: validity pattern exhausted"
              v0 : rest0 ->
                let (v1, rest1) =
                      if remaining P.> 1
                        then case rest0 of
                          v : rs -> (P.Just v, rs)
                          [] -> (P.Nothing, [])
                        else (P.Nothing, rest0)
                    (v2, _rest2) =
                      if remaining P.> 2
                        then case rest1 of
                          v : rs -> (P.Just v, rs)
                          [] -> (P.Nothing, [])
                        else (P.Nothing, rest1)
                    avail = if remaining P.< lookN then remaining else lookN
                    v1b = case v1 of
                      P.Just v -> v
                      P.Nothing -> P.False
                    v2b = case v2 of
                      P.Just v -> v
                      P.Nothing -> P.False
                    (consumeCount, emittedThis, buffer') = case lookahead of
                      SampleNTT512.Lookahead0 -> decide0 buffer v0 v1b
                      SampleNTT512.Lookahead1 -> decide1 buffer v0 v1b v2b
                    consumeN' = if consumeCount P.> avail then avail else consumeCount
                    restVals = P.drop consumeN' vals
                    (out, pairs', emitted') =
                      if emittedThis
                        then case pairs of
                          p : ps -> (Just p, ps, emitted P.+ 1)
                          [] -> P.error "SampleNTT512.simulate: output exhausted"
                        else (Nothing, pairs, emitted)
                 in runBlock restVals (remaining P.- consumeN') buffer' pairs' emitted' (out : acc)

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
