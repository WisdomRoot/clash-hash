{-# LANGUAGE DataKinds #-}

module Test.SampleNTT (spec) where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude (BitVector, bundle, clockGen, enableGen, fromList, resetGen, sampleN, (++#))
import Component.SampleNTT qualified as SampleNTT
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
spec = describe "SN-O24-L2" $
  runAllTests 2 5 SampleNTT.i272o24l2
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
          withMaxSuccess 20 $ forAll Samples.genSampleNTTTest (runStreamTestWith (simulate lookaheadCount bufferSize) topEntityCore)

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
          (packedBytes, _) = getSampleNTTOutput seed
          expectedValues = coeffPairsFromPacked packedBytes
          baselineNoBpLen = P.length (expandOutputTiming (expectedFn seed [Ready 1] inputTiming))
       in runStreamInputExpected topEntity expectedValues baselineNoBpLen inputTiming backpressureTiming

simulate :: P.Int -> P.Int -> ByteString -> BackpressureTiming -> InputTiming 272 -> OutputTiming 24
simulate lookaheadCount bufferSize seed backpressureTiming inputTiming =
  if bufferSize P.== 1 P.&& (lookaheadCount P.== 0 P.|| lookaheadCount P.== 1)
    then simulateL01
    else
      if lookaheadCount P.== 2
        then simulateL2
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
          base = compress (idleOut P.++ permuteOut P.++ squeezeOut)
       in base

    simulateL2 =
      let (inputPattern, _) = expandInputTiming inputTiming
          startSilence =
            case L.findIndex isJust inputPattern of
              Just i -> i
              Nothing -> P.error "SampleNTT.simulateL2: no input provided"
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
    assignCandidates [] _ = P.error "SampleNTT.simulateL2: extra coefficients"
    assignCandidates (v : vs) coeffs' =
      if v
        then case coeffs' of
          [] -> P.error "SampleNTT.simulateL2: ran out of coefficients"
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
          [] ->
            if P.length buffer P.< 2
              then P.error "SampleNTT.simulateL2: candidate blocks exhausted"
              else
                let (permuteOut, rs') = consumeN 24 rs
                    (drainOut, buffer', rs'', emitted') = drainBufferL2 buffer rs' emitted
                 in if emitted' P.>= 128
                      then
                        if P.null buffer'
                          then (permuteOut P.++ drainOut, rs'')
                          else P.error "SampleNTT.simulateL2: extra buffered coefficients after final drain"
                      else P.error "SampleNTT.simulateL2: final buffer drain incomplete"
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
              [] -> P.error "SampleNTT.simulateL2: empty backpressure pattern"
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
                _ -> P.error "SampleNTT.simulateL2: invalid candidate chunk size"

    drainBufferL2 ::
      [Word16] ->
      [P.Bool] ->
      P.Int ->
      ([P.Maybe (BitVector 24)], [Word16], [P.Bool], P.Int)
    drainBufferL2 buffer rs emitted
      | emitted P.>= 128 = ([], buffer, rs, emitted)
      | P.length buffer P.< 2 = ([], buffer, rs, emitted)
      | P.otherwise =
          case rs of
            [] -> P.error "SampleNTT.simulateL2: empty backpressure pattern during final drain"
            r : rs' ->
              let a = buffer P.!! 0
                  b = buffer P.!! 1
               in if r
                    then
                      let (out, buffer', rs'', emitted') =
                            drainBufferL2 (P.drop 2 buffer) rs' (emitted P.+ 1)
                       in (P.Just (mkPair a b) : out, buffer', rs'', emitted')
                    else
                      let (out, buffer', rs'', emitted') =
                            drainBufferL2 buffer rs' emitted
                       in (P.Nothing : out, buffer', rs'', emitted')

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
  [BitVector 24] ->
  P.Int ->
  InputTiming 272 ->
  BackpressureTiming ->
  P.IO ()
runStreamInputExpected topEntity expectedValues baselineNoBpLen inputTiming backpressureTiming = do
  let expectedCount = P.length expectedValues
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> P.repeat P.True
        _ -> P.cycle readyPattern
      (inputPattern, _inputValues) = expandInputTiming inputTiming
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "SampleNTT Stream: no input provided"
      patternLen = P.max 1 (P.length readyPattern)
      readyCount = P.max 1 (P.length (P.filter P.id readyPattern))
      readyBudget = ceilDiv (expectedCount P.* patternLen) readyCount
      sampleLen = P.max (baselineNoBpLen P.+ 300) (startSilence P.+ 25 P.+ readyBudget P.+ 300)
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
      samples = sampleN sampleLen (bundle (output, treadySignal, inputSignal))
      handshakes =
        [ tdata stream
          | ((stream, _), ready, _) <- P.drop 1 samples,
            tvalid stream P.&& ready
        ]
      outSamples =
        [ (stream, ready)
          | ((stream, _), ready, _) <- P.drop 1 samples
        ]
      outHoldViolations =
        [ idx
          | (idx, ((prevStream, prevReady), (currStream, _))) <-
              P.zip ([1 ..] :: [P.Int]) (P.zip outSamples (P.drop 1 outSamples)),
            tvalid prevStream P.&& P.not prevReady,
            tvalid currStream P./= P.True
              P.|| tdata currStream P./= tdata prevStream
              P.|| tlast currStream P./= tlast prevStream
        ]
      inputValids = [tvalid inStream | (_, _, inStream) <- samples]
      readyPrev = P.True : [seedReady | ((_, seedReady), _, _) <- P.init samples]
      readyViolations = [() | (valid, ready) <- P.zip inputValids readyPrev, valid P.&& P.not ready]
      inputHandshakes = [() | (valid, ready) <- P.zip inputValids readyPrev, valid P.&& ready]
      actualPrefix = P.take expectedCount handshakes
      protocolError =
        if P.null readyViolations
          then
            if P.null outHoldViolations
              then P.Nothing
              else
                P.Just
                  ( "SampleNTT Stream: output changed during backpressure at cycle "
                      P.++ P.show (P.head outHoldViolations)
                  )
          else P.Just "SampleNTT Stream: MSG_TVALID asserted without MSG_TREADY in previous cycle"
   in case protocolError of
        P.Just err -> P.error err
        P.Nothing ->
          if P.null inputHandshakes
            then P.error "SampleNTT Stream: MSG_TVALID never asserted after MSG_TREADY"
            else
              if P.length handshakes P.< expectedCount
                then
                  P.error
                    ( "SampleNTT Stream: did not complete 128 outputs before timeout; got "
                        P.++ P.show (P.length handshakes)
                        P.++ ", expected "
                        P.++ P.show expectedCount
                        P.++ ", sampleLen="
                        P.++ P.show sampleLen
                    )
                else actualPrefix `shouldBe` expectedValues
  where
    ceilDiv x y = (x P.+ y P.- 1) `P.div` y
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

coeffPairsFromPacked :: ByteString -> [BitVector 24]
coeffPairsFromPacked packed = go (unpackPython384Bytes packed)
  where
    go (a : b : rest) =
      let a12 = P.fromIntegral a :: BitVector 12
          b12 = P.fromIntegral b :: BitVector 12
       in (b12 ++# a12) : go rest
    go _ = []
