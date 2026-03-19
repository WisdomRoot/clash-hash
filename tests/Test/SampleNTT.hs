{-# LANGUAGE DataKinds #-}

module Test.SampleNTT (spec, specL2, specL4, specL6) where

import AXI4Stream (AXI4Stream (..), Pipe)
import Clash.Prelude (BitVector, System, clockGen, enableGen, resetGen, withClockResetEnable, (++#))
import Component.SampleNTT qualified as SampleNTT
import Component.SampleNTT4 qualified as SampleNTT4
import Component.SampleNTT6 qualified as SampleNTT6
import Component.XOF6 qualified as XOF6
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
spec = specL2

specL2 :: Spec
specL2 = sampleNTTSpec "SN-O24-L2" 2 5 i272o24l2AsPipe

specL4 :: Spec
specL4 = sampleNTTSpecL4 "SN-O24-L4"

specL6 :: Spec
specL6 = sampleNTTSpec "SN-O24-L6" 6 9 i272o24l6AsPipe

i272o24l2AsPipe :: Pipe System 272 24
i272o24l2AsPipe args =
  withClockResetEnable clockGen resetGen enableGen (SampleNTT.i272o24l2Core args)

i272o24l6AsPipe :: Pipe System 272 24
i272o24l6AsPipe args =
  withClockResetEnable clockGen resetGen enableGen (SampleNTT6.i272o24l6Core args)

sampleNTTSpecL4 :: P.String -> Spec
sampleNTTSpecL4 name =
  describe name $ do
    describe "Basic functionality tests (34-byte seeds)" $
      for_ Samples.basicSeedCases $ \testCase ->
        it (testLabel testCase) $ runStepTestWith (simulate 4 7) testCase
    describe "Upstream stall handling (34-byte seeds)" $
      for_ Samples.stallSeedCases $ \testCase ->
        it (testLabel testCase) $ runStepTestWith (simulate 4 7) testCase
    describe "Downstream backpressure handling (34-byte seeds)" $
      for_ Samples.backpressureSeedCases $ \testCase ->
        it (testLabel testCase) $ runStepTestWith (simulate 4 7) testCase
    describe "Combined stress tests (34-byte seeds)" $
      for_ Samples.combinedSeedCases $ \testCase ->
        it (testLabel testCase) $ runStepTestWith (simulate 4 7) testCase
    describe "QuickCheck property tests (34-byte seeds)" $
      it "correctly handles random 34-byte test cases" $
        withMaxSuccess 20 $ forAll Samples.genSampleNTTTest (runStepTestWith (simulate 4 7))

sampleNTTSpec ::
  P.String ->
  P.Int ->
  P.Int ->
  Pipe System 272 24 ->
  Spec
sampleNTTSpec name lookaheadCount bufferSize pipeEntity =
  describe name $ do
    describe "Basic functionality tests (34-byte seeds)" $
      for_ Samples.basicSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipeTestWith (simulate lookaheadCount bufferSize) pipeEntity testCase
    describe "Upstream stall handling (34-byte seeds)" $
      for_ Samples.stallSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipeTestWith (simulate lookaheadCount bufferSize) pipeEntity testCase
    describe "Downstream backpressure handling (34-byte seeds)" $
      for_ Samples.backpressureSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipeTestWith (simulate lookaheadCount bufferSize) pipeEntity testCase
    describe "Combined stress tests (34-byte seeds)" $
      for_ Samples.combinedSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipeTestWith (simulate lookaheadCount bufferSize) pipeEntity testCase
    describe "QuickCheck property tests (34-byte seeds)" $
      it "correctly handles random 34-byte test cases" $
        withMaxSuccess 20 $ forAll Samples.genSampleNTTTest (runPipeTestWith (simulate lookaheadCount bufferSize) pipeEntity)

runPipeTestWith ::
  (ByteString -> BackpressureTiming -> InputTiming 272 -> OutputTiming 24) ->
  Pipe System 272 24 ->
  ShakeTest ->
  P.IO ()
runPipeTestWith expectedFn pipeEntity testCase =
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
      simulateCase inputTiming' backpressureTiming' =
        expectedFn seed backpressureTiming' inputTiming'
   in runPipeInputExact pipeEntity simulateCase inputTiming backpressureTiming

runStepTestWith ::
  (ByteString -> BackpressureTiming -> InputTiming 272 -> OutputTiming 24) ->
  ShakeTest ->
  P.IO ()
runStepTestWith expectedFn testCase =
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
      expectedBase = expandOutputTiming (expectedFn seed backpressureTiming inputTiming)
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> P.repeat P.True
        _ -> P.cycle readyPattern
      actual = simulateL4DUT (P.length expectedBase) (P.fst (expandInputTiming inputTiming)) readyStream
   in actual `shouldBe` expectedBase

simulateL4DUT ::
  P.Int ->
  [Maybe (BitVector 272)] ->
  [P.Bool] ->
  [Maybe (BitVector 24)]
simulateL4DUT sampleLen inputPattern readyStream =
  go sampleLen annotatedInputs readyStream (XOF6.State XOF6.Absorb 0) SampleNTT4.Buffer0 (SampleNTT4.PairCount 0)
  where
    lastJustIdx =
      case [i | (i, Just _) <- P.zip ([0 ..] :: [P.Int]) inputPattern] of
        [] -> Nothing
        xs -> Just (P.last xs)

    annotatedInputs =
      P.zipWith
        (\i mv -> P.fmap (\v -> (v, Just i P.== lastJustIdx)) mv)
        ([0 ..] :: [P.Int])
        inputPattern

    go 0 _ _ _ _ _ = []
    go _ _ [] _ _ _ = P.error "Test.SampleNTT.simulateL4DUT: empty ready stream"
    go n inputs (coeffReady : restReady) xofState lookState takeState =
      let inputBeat = currentInputBeat inputs
          (_xofStallState, (_seedReady, candidateStream)) = XOF6.step xofState (P.False, inputBeat)
          (lookStateFalse, (xofReadyFalse, coeffStreamFalse)) =
            SampleNTT4.stepLookahead4 lookState (P.False, candidateStream)
          (_, (midReadyFalse, _)) =
            SampleNTT4.stepTake128 takeState (coeffReady, coeffStreamFalse)
          (lookStateTrue, (xofReadyTrue, coeffStreamTrue)) =
            SampleNTT4.stepLookahead4 lookState (P.True, candidateStream)
          (_, (midReadyTrue, _)) =
            SampleNTT4.stepTake128 takeState (coeffReady, coeffStreamTrue)
          (_midReady, lookState', xofReady, coeffStream) =
            if midReadyFalse P.== P.False
              then (P.False, lookStateFalse, xofReadyFalse, coeffStreamFalse)
              else
                if midReadyTrue P.== P.True
                  then (P.True, lookStateTrue, xofReadyTrue, coeffStreamTrue)
                  else P.error "Test.SampleNTT.simulateL4DUT: no ready fixed point"
          (takeState', (_, outStream)) =
            SampleNTT4.stepTake128 takeState (coeffReady, coeffStream)
          (xofState', (seedReady', _)) =
            XOF6.step xofState (xofReady, inputBeat)
          inputs' = advanceInputs seedReady' inputs
          outBeat =
            if tvalid outStream P.&& coeffReady
              then P.Just (tdata outStream)
              else P.Nothing
       in outBeat : go (n P.- 1) inputs' restReady xofState' lookState' takeState'

    currentInputBeat [] = AXI4Stream 0 P.False P.False
    currentInputBeat (P.Nothing : _) = AXI4Stream 0 P.False P.False
    currentInputBeat (P.Just (v, isLast) : _) = AXI4Stream v P.True isLast

    advanceInputs _ [] = []
    advanceInputs _ (P.Nothing : rest) = rest
    advanceInputs seedReady inputs@(P.Just _ : rest) =
      if seedReady then rest else inputs

simulate :: P.Int -> P.Int -> ByteString -> BackpressureTiming -> InputTiming 272 -> OutputTiming 24
simulate lookaheadCount bufferSize seed backpressureTiming inputTiming =
  if bufferSize P.== 1 P.&& (lookaheadCount P.== 0 P.|| lookaheadCount P.== 1)
    then simulateL01
    else
      if lookaheadCount P.>= 2
        then simulateBuffered
        else P.error "SampleNTT.simulate: unsupported lookahead/bufferSize"
  where
    simulateL01 =
      let (inputPattern, _) = expandInputTiming inputTiming
          startSilence =
            case L.findIndex isJust inputPattern of
              Just i -> i
              Nothing -> P.error "SampleNTT.simulate: no input provided"
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
       in compress (idleOut P.++ permuteOut P.++ squeezeOut)

    simulateBuffered =
      let (inputPattern, _) = expandInputTiming inputTiming
          startSilence =
            case L.findIndex isJust inputPattern of
              Just i -> i
              Nothing -> P.error "SampleNTT.simulateBuffered: no input provided"
          (packedBytes, validityRaw) = getSampleNTTOutput seed
          coeffs = unpackPython384Bytes packedBytes
          chunkWidth = lookaheadCount P.+ 2
          expectedBufferSize = chunkWidth P.+ 1
          candidates = assignCandidates validityRaw coeffs
          blocks =
            if lookaheadCount P.== 4
              then buildL4Blocks candidates
              else
                let chunksPerBlock = (112 P.+ chunkWidth P.- 1) `P.div` chunkWidth
                    paddedBlockCandidates = chunksPerBlock P.* chunkWidth
                 in buildBufferedBlocks 112 paddedBlockCandidates chunkWidth candidates
          readyPattern = expandBackpressureTiming backpressureTiming
          readyStream = case readyPattern of
            [] -> P.repeat P.True
            _ -> P.cycle readyPattern
          (idleOut, readyAfterIdle) = consumeN startSilence readyStream
          (permuteOut, readyAfterPermute) = consumeN 25 readyAfterIdle
          (squeezeOut, _) = runBlocksBuffered blocks [] readyAfterPermute 0
       in if bufferSize P./= expectedBufferSize
            then P.error "SampleNTT.simulateBuffered: bufferSize mismatch"
            else compress (idleOut P.++ permuteOut P.++ squeezeOut)

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
              [] -> P.error "SampleNTT.simulate: validity pattern exhausted"
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
                    avail = P.min remaining lookN
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
                        _ -> P.error "SampleNTT.simulate: unsupported lookahead"
                    consumeN' = P.min consumeCount avail
                    restVals = P.drop consumeN' vals
                    (out, pairs', emitted') =
                      if emittedThis
                        then case pairs of
                          p : ps -> (Just p, ps, emitted P.+ 1)
                          [] -> P.error "SampleNTT.simulate: output exhausted"
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
        [] -> P.error "SampleNTT.simulate: empty backpressure pattern"

    consumeN :: P.Int -> [P.Bool] -> ([Maybe (BitVector 24)], [P.Bool])
    consumeN n rs =
      case n of
        0 -> ([], rs)
        _ ->
          case rs of
            _ : rs' ->
              let (out, rs'') = consumeN (n P.- 1) rs'
               in (Nothing : out, rs'')
            [] -> P.error "SampleNTT.simulate: empty backpressure pattern"

    chunksOf n xs =
      case P.splitAt n xs of
        ([], _) -> []
        (chunk, rest) -> chunk : chunksOf n rest

    buildBufferedBlocks realPerBlock paddedPerBlock chunkWidth candidates
      | P.null candidates = []
      | P.otherwise =
          let (realBlock, rest) = P.splitAt realPerBlock candidates
              paddedBlock = realBlock P.++ P.replicate (paddedPerBlock P.- P.length realBlock) P.Nothing
           in chunksOf chunkWidth paddedBlock : buildBufferedBlocks realPerBlock paddedPerBlock chunkWidth rest

    buildL4Blocks candidates = go candidates (P.cycle [18, 19, 19])
      where
        go candidates' phases
          | P.null candidates' = []
          | P.otherwise =
              let phaseChunks = P.head phases
                  (phaseCandidates, rest) = P.splitAt (phaseChunks P.* 6) candidates'
               in chunksOf 6 phaseCandidates : go rest (P.tail phases)

    assignCandidates [] [] = []
    assignCandidates [] _ = P.error "SampleNTT.simulateBuffered: extra coefficients"
    assignCandidates (v : vs) coeffs' =
      if v
        then case coeffs' of
          [] -> P.error "SampleNTT.simulateBuffered: ran out of coefficients"
          c : cs -> P.Just c : assignCandidates vs cs
        else P.Nothing : assignCandidates vs coeffs'

    runBlocksBuffered ::
      [[[P.Maybe Word16]]] ->
      [Word16] ->
      [P.Bool] ->
      P.Int ->
      ([P.Maybe (BitVector 24)], [P.Bool])
    runBlocksBuffered blocks buffer rs emitted =
      if emitted P.>= 128
        then ([], rs)
        else case blocks of
          [] ->
            let (gapOut, buffer', rs', emitted') = runGapBuffered 24 buffer rs emitted
             in if emitted' P.>= 128
                  then (gapOut, rs')
                  else
                    if P.length buffer' P.< 2
                      then P.error "SampleNTT.simulateBuffered: candidate blocks exhausted"
                      else P.error "SampleNTT.simulateBuffered: final buffer drain incomplete"
          block : rest ->
            let (blockOut, buffer', rs', emitted') = runBlockBuffered block buffer rs emitted
             in if emitted' P.>= 128
                  then (blockOut, rs')
                  else
                    let (gapOut, buffer'', rs'', emitted'') = runGapBuffered 24 buffer' rs' emitted'
                     in if emitted'' P.>= 128
                          then (blockOut P.++ gapOut, rs'')
                          else
                            let (moreOut, rs''') = runBlocksBuffered rest buffer'' rs'' emitted''
                             in (blockOut P.++ gapOut P.++ moreOut, rs''')

    runBlockBuffered ::
      [[P.Maybe Word16]] ->
      [Word16] ->
      [P.Bool] ->
      P.Int ->
      ([P.Maybe (BitVector 24)], [Word16], [P.Bool], P.Int)
    runBlockBuffered block buffer rs emitted = go 0 buffer rs emitted []
      where
        blockLen = P.length block
        go idx buf ready emitted' acc
          | emitted' P.>= 128 = (P.reverse acc, buf, ready, emitted')
          | idx P.>= blockLen = (P.reverse acc, buf, ready, emitted')
          | P.otherwise =
              case ready of
                [] -> P.error "SampleNTT.simulateBuffered: empty backpressure pattern"
                r : rs' ->
                  let chunk = block P.!! idx
                      (outMaybe, buf', advanceIdx, produced) = stepBuffered buf chunk r
                      idx' = if advanceIdx then idx P.+ 1 else idx
                      emitted'' = emitted' P.+ produced
                   in go idx' buf' rs' emitted'' (outMaybe : acc)

    stepBuffered ::
      [Word16] ->
      [P.Maybe Word16] ->
      P.Bool ->
      (P.Maybe (BitVector 24), [Word16], P.Bool, P.Int)
    stepBuffered buffer chunk tready =
      case buffer of
        a : b : rest ->
          if tready
            then (P.Just (mkPair a b), rest, P.False, 1)
            else (P.Nothing, buffer, P.False, 0)
        [b0] ->
          let vals = catMaybes chunk
           in case vals of
                [] -> (P.Nothing, [b0], P.True, 0)
                c0 : restVals ->
                  if tready
                    then (P.Just (mkPair b0 c0), restVals, P.True, 1)
                    else (P.Nothing, b0 : vals, P.True, 0)
        [] ->
          let vals = catMaybes chunk
           in case vals of
                [] -> (P.Nothing, [], P.True, 0)
                [c0] -> (P.Nothing, [c0], P.True, 0)
                c0 : c1 : restVals ->
                  if tready
                    then (P.Just (mkPair c0 c1), restVals, P.True, 1)
                    else (P.Nothing, vals, P.True, 0)

    runGapBuffered ::
      P.Int ->
      [Word16] ->
      [P.Bool] ->
      P.Int ->
      ([P.Maybe (BitVector 24)], [Word16], [P.Bool], P.Int)
    runGapBuffered n buffer rs emitted
      | n P.<= 0 = ([], buffer, rs, emitted)
      | emitted P.>= 128 = ([], buffer, rs, emitted)
      | P.otherwise =
          case rs of
            [] -> P.error "SampleNTT.simulateBuffered: empty backpressure pattern during permute gap"
            r : rs' ->
              let (outMaybe, buffer', produced) =
                    case buffer of
                      a : b : rest ->
                        if r
                          then (P.Just (mkPair a b), rest, 1)
                          else (P.Nothing, buffer, 0)
                      _ -> (P.Nothing, buffer, 0)
                  (out, buffer'', rs'', emitted') =
                    runGapBuffered (n P.- 1) buffer' rs' (emitted P.+ produced)
               in (outMaybe : out, buffer'', rs'', emitted')

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
