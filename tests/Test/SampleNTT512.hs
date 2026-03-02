{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.SampleNTT512 (spec) where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude (BitVector, bundle, clockGen, enableGen, fromList, resetGen, sampleN, (++#))
import Component.SampleNTT512 qualified as SampleNTT512
import Component.SampleNTT512B qualified as SampleNTT512B
import Component.SampleNTT512N qualified as SampleNTT512N
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Maybe (catMaybes, isJust)
import Data.Word (Word16)
import Stream
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (forAll, withMaxSuccess)
import Test.TestHarness.SampleNTT.Common
  ( ShakeTest (..),
    UpstreamStall (..),
    backpressurePattern,
    bsToBV272Normal,
    getSampleNTTOutput,
    testLabel,
    unpackPython384Bytes,
  )
import Test.TestHarness.SampleNTTSamples qualified as Samples
import Prelude (Maybe (..), ($))
import Prelude qualified as P

spec :: Spec
spec = describe "SampleNTT512 Stream" $ do
  describe "i272o24l0" $ runAllTests 0 1 SampleNTT512.i272o24l0
  describe "i272o24l1" $ runAllTests 1 1 (SampleNTT512.i272o24l1 SampleNTT512.Lookahead1)
  describe "i272o24l2" $ runAllTests 2 5 SampleNTT512N.i272o24l2Top
  describe "i272o24b60" $ runAllTests 2 5 SampleNTT512B.topEntity
  where
    runAllTests lookaheadCount bufferSize topEntityCore = do
      describe "Basic functionality tests (34-byte seeds)" $
        for_ Samples.basicSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTestWith (simulate lookaheadCount bufferSize) topEntityCore testCase
      describe "Upstream stall handling (34-byte seeds)" $
        for_ Samples.stallSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTestWith (simulate lookaheadCount bufferSize) topEntityCore testCase
      describe "Downstream backpressure handling (34-byte seeds)" $
        for_ Samples.backpressureSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTestWith (simulate lookaheadCount bufferSize) topEntityCore testCase
      describe "Combined stress tests (34-byte seeds)" $
        for_ Samples.combinedSeedCases $ \testCase ->
          it (testLabel testCase) $ runStreamTestWith (simulate lookaheadCount bufferSize) topEntityCore testCase
      describe "QuickCheck property tests (34-byte seeds)" $
        it "correctly handles random 34-byte test cases" $
          withMaxSuccess 40 $ forAll Samples.genSampleNTTTest (runStreamTestWith (simulate lookaheadCount bufferSize) topEntityCore)

    runStreamTestWith expectedFn topEntityCore testCase =
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
          expected = expectedFn seed backpressureTiming inputTiming
       in runStreamInputExpected topEntity expected inputTiming backpressureTiming

simulate :: P.Int -> P.Int -> ByteString -> BackpressureTiming -> InputTiming 272 -> OutputTiming 24
simulate lookaheadCount bufferSize seed backpressureTiming inputTiming =
  if bufferSize P.== 1 P.&& (lookaheadCount P.== 0 P.|| lookaheadCount P.== 1)
    then simulateL01
    else
      if lookaheadCount P.== 2
        then simulateL2
        else P.error "SampleNTT512.simulate: unsupported lookahead/bufferSize"
  where
    simulateL01 =
      let (inputPattern, _) = expandInputTiming inputTiming
          startSilence =
            case L.findIndex isJust inputPattern of
              Just i -> i
              Nothing -> P.error "SampleNTT512.simulate: no input provided"
          (packedBytes, validityRaw) = getSampleNTTOutput seed
          validity =
            if P.odd (P.length validityRaw)
              then validityRaw P.++ [P.False]
              else validityRaw
          coeffs = unpackPython384Bytes packedBytes
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

    simulateL2 =
      let (inputPattern, _) = expandInputTiming inputTiming
          startSilence =
            case L.findIndex isJust inputPattern of
              Just i -> i
              Nothing -> P.error "SampleNTT512.simulateL2: no input provided"
          (packedBytes, validityRaw) = getSampleNTTOutput seed
          coeffs = unpackPython384Bytes packedBytes
          validity = padToMultiple P.False 4 validityRaw
          candidates = assignCandidates validity coeffs
          chunks = chunksOf 4 (padToMultiple P.Nothing 4 candidates)
          blocks = chunksOf 28 (padChunks 28 chunks)
          readyPattern = expandBackpressureTiming backpressureTiming
          readyStream = case readyPattern of
            [] -> P.repeat P.True
            _ -> P.cycle readyPattern
          (idleOut, readyAfterIdle) = consumeN startSilence readyStream
          (permuteOut, readyAfterPermute) = consumeN 25 readyAfterIdle
          (squeezeOut, _) = runBlocksL2 blocks [] readyAfterPermute 0
          base = compress (idleOut P.++ permuteOut P.++ squeezeOut)
       in base

    toPairs (a : b : rest) = (toBV12 b ++# toBV12 a) : toPairs rest
    toPairs _ = []

    toBV12 v = P.fromIntegral v :: BitVector 12

    lookN = lookaheadCount P.+ 2

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
                    (consumeCount, emittedThis, buffer') =
                      case lookaheadCount of
                        0 -> decide0 buffer v0 v1b
                        1 -> decide1 buffer v0 v1b v2b
                        _ -> P.error "SampleNTT512.simulate: unsupported lookahead"
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

    padToMultiple filler n xs =
      let r = P.length xs `P.mod` n
          pad = if r P.== 0 then 0 else n P.- r
       in xs P.++ P.replicate pad filler

    padChunks n xs =
      let r = P.length xs `P.mod` n
          pad = if r P.== 0 then 0 else n P.- r
          filler = P.replicate 4 P.Nothing
       in xs P.++ P.replicate pad filler

    chunksOf n xs =
      case P.splitAt n xs of
        ([], _) -> []
        (chunk, rest) -> chunk : chunksOf n rest

    assignCandidates [] [] = []
    assignCandidates [] _ = P.error "SampleNTT512.simulateL2: extra coefficients"
    assignCandidates (v : vs) coeffs' =
      if v
        then case coeffs' of
          [] -> P.error "SampleNTT512.simulateL2: ran out of coefficients"
          c : cs -> P.Just c : assignCandidates vs cs
        else P.Nothing : assignCandidates vs coeffs'

    runBlocksL2 ::
      [[[P.Maybe Word16]]] ->
      [Word16] ->
      [P.Bool] ->
      P.Int ->
      ([P.Maybe (BitVector 24)], [P.Bool])
    runBlocksL2 blocks buffer rs emitted =
      if emitted P.>= 128
        then ([], rs)
        else case blocks of
          [] -> P.error "SampleNTT512.simulateL2: candidate blocks exhausted"
          block : rest ->
            let (blockOut, buffer', rs', emitted') = runBlockL2 block buffer rs emitted
             in if emitted' P.>= 128
                  then (blockOut, rs')
                  else
                    let (permuteOut, rs'') = consumeN 24 rs'
                        (moreOut, rs''') = runBlocksL2 rest buffer' rs'' emitted'
                     in (blockOut P.++ permuteOut P.++ moreOut, rs''')

    runBlockL2 ::
      [[P.Maybe Word16]] ->
      [Word16] ->
      [P.Bool] ->
      P.Int ->
      ([P.Maybe (BitVector 24)], [Word16], [P.Bool], P.Int)
    runBlockL2 block buffer rs emitted = go 0 buffer rs emitted []
      where
        blockLen = P.length block
        go idx buf ready emitted' acc
          | emitted' P.>= 128 = (P.reverse acc, buf, ready, emitted')
          | idx P.>= blockLen = (P.reverse acc, buf, ready, emitted')
          | P.otherwise =
            case ready of
              [] -> P.error "SampleNTT512.simulateL2: empty backpressure pattern"
              r : rs' ->
                let chunk = block P.!! idx
                    (rawOut, buf', advanceIdx, produced) = stepL2 buf chunk r
                    outMaybe = if r then rawOut else P.Nothing
                    idx' = if advanceIdx then idx P.+ 1 else idx
                    emitted'' = emitted' P.+ produced
                 in go idx' buf' rs' emitted'' (outMaybe : acc)

    stepL2 ::
      [Word16] ->
      [P.Maybe Word16] ->
      P.Bool ->
      (P.Maybe (BitVector 24), [Word16], P.Bool, P.Int)
    stepL2 buffer chunk tready =
      case buffer of
        a : b : rest -> (P.Just (mkPair a b), rest, P.False, 1)
        [b0] ->
          let vals = catMaybes chunk
           in case vals of
                [] -> (P.Nothing, [b0], P.True, 0)
                c0 : restVals ->
                  if tready
                    then (P.Just (mkPair b0 c0), restVals, P.True, 1)
                    else
                      if P.length (b0 : vals) P.<= bufferSize
                        then (P.Nothing, b0 : vals, P.True, 0)
                        else (P.Nothing, [b0], P.False, 0)
        [] ->
          let vals = catMaybes chunk
           in case vals of
                [] -> (P.Nothing, [], P.True, 0)
                [c0] -> (P.Nothing, [c0], P.True, 0)
                [c0, c1] ->
                  if tready
                    then (P.Just (mkPair c0 c1), [], P.True, 1)
                    else
                      if P.length vals P.<= bufferSize
                        then (P.Nothing, vals, P.True, 0)
                        else (P.Nothing, [], P.False, 0)
                [c0, c1, c2] ->
                  if tready
                    then (P.Just (mkPair c0 c1), [c2], P.True, 1)
                    else
                      if P.length vals P.<= bufferSize
                        then (P.Nothing, vals, P.True, 0)
                        else (P.Nothing, [], P.False, 0)
                [c0, c1, c2, c3] ->
                  if tready
                    then (P.Just (mkPair c0 c1), [c2, c3], P.True, 1)
                    else
                      if P.length vals P.<= bufferSize
                        then (P.Nothing, vals, P.True, 0)
                        else (P.Nothing, [], P.False, 0)
                _ -> P.error "SampleNTT512.simulateL2: invalid candidate chunk size"

    mkPair a b = toBV12 b ++# toBV12 a

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
