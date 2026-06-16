{-# LANGUAGE DataKinds #-}

module Test.SampleNTT (spec, specL2, specL4, specL6, specL6O48) where

import AXI4Stream (Pipe, Pipe2)
import Clash.Prelude (BitVector, System, clockGen, enableGen, resetGen, withClockResetEnable, (++#))
import Component.SampleNTT qualified as SampleNTT
import Component.SampleNTT4 qualified as SampleNTT4
import Component.SampleNTT6 qualified as SampleNTT6
import Component.SampleNTT6O48 qualified as SampleNTT6O48
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Maybe (catMaybes, isJust)
import Data.Word (Word16)
import Stream
import Test.Hspec (Spec, describe, it)
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
specL4 = sampleNTTSpec2 "SN-O24-L4" 4 7 SampleNTT4.i272o24l4Core

specL6 :: Spec
specL6 = sampleNTTSpec "SN-O24-L6" 6 9 i272o24l6AsPipe

specL6O48 :: Spec
specL6O48 = sampleNTTO48Spec "SN-O48-L6" i272o48l6AsPipe

i272o24l2AsPipe :: Pipe System 272 24
i272o24l2AsPipe args =
  withClockResetEnable clockGen resetGen enableGen (SampleNTT.i272o24l2Core args)

i272o24l6AsPipe :: Pipe System 272 24
i272o24l6AsPipe args =
  withClockResetEnable clockGen resetGen enableGen (SampleNTT6.i272o24l6Core args)

i272o48l6AsPipe :: Pipe System 272 48
i272o48l6AsPipe args =
  withClockResetEnable clockGen resetGen enableGen (SampleNTT6O48.i272o48l6Core args)

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

sampleNTTSpec2 ::
  P.String ->
  P.Int ->
  P.Int ->
  Pipe2 System 272 24 ->
  Spec
sampleNTTSpec2 name lookaheadCount bufferSize pipeEntity =
  describe name $ do
    describe "Basic functionality tests (34-byte seeds)" $
      for_ Samples.basicSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipe2TestWith (simulate lookaheadCount bufferSize) pipeEntity testCase
    describe "Upstream stall handling (34-byte seeds)" $
      for_ Samples.stallSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipe2TestWith (simulate lookaheadCount bufferSize) pipeEntity testCase
    describe "Downstream backpressure handling (34-byte seeds)" $
      for_ Samples.backpressureSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipe2TestWith (simulate lookaheadCount bufferSize) pipeEntity testCase
    describe "Combined stress tests (34-byte seeds)" $
      for_ Samples.combinedSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipe2TestWith (simulate lookaheadCount bufferSize) pipeEntity testCase
    describe "QuickCheck property tests (34-byte seeds)" $
      it "correctly handles random 34-byte test cases" $
        withMaxSuccess 20 $ forAll Samples.genSampleNTTTest (runPipe2TestWith (simulate lookaheadCount bufferSize) pipeEntity)

sampleNTTO48Spec ::
  P.String ->
  Pipe System 272 48 ->
  Spec
sampleNTTO48Spec name pipeEntity =
  describe name $ do
    describe "Basic functionality tests (34-byte seeds)" $
      for_ Samples.basicSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipeTestWithO48 simulateO48L6 pipeEntity testCase
    describe "Upstream stall handling (34-byte seeds)" $
      for_ Samples.stallSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipeTestWithO48 simulateO48L6 pipeEntity testCase
    describe "Downstream backpressure handling (34-byte seeds)" $
      for_ Samples.backpressureSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipeTestWithO48 simulateO48L6 pipeEntity testCase
    describe "Combined stress tests (34-byte seeds)" $
      for_ Samples.combinedSeedCases $ \testCase ->
        it (testLabel testCase) $ runPipeTestWithO48 simulateO48L6 pipeEntity testCase
    describe "QuickCheck property tests (34-byte seeds)" $
      it "correctly handles random 34-byte test cases" $
        withMaxSuccess 20 $ forAll Samples.genSampleNTTTest (runPipeTestWithO48 simulateO48L6 pipeEntity)

runPipe2TestWith ::
  (ByteString -> BackpressureTiming -> InputTiming 272 -> OutputTiming 24) ->
  Pipe2 System 272 24 ->
  ShakeTest ->
  P.IO ()
runPipe2TestWith expectedFn pipeEntity testCase =
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
   in runPipe2InputExact pipeEntity simulateCase inputTiming backpressureTiming

runPipeTestWithO48 ::
  (ByteString -> BackpressureTiming -> InputTiming 272 -> OutputTiming 48) ->
  Pipe System 272 48 ->
  ShakeTest ->
  P.IO ()
runPipeTestWithO48 expectedFn pipeEntity testCase =
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

simulateO48L6 :: ByteString -> BackpressureTiming -> InputTiming 272 -> OutputTiming 48
simulateO48L6 seed backpressureTiming inputTiming =
  let (inputPattern, _) = expandInputTiming inputTiming
      startSilence =
        case L.findIndex isJust inputPattern of
          Just i -> i
          Nothing -> P.error "SampleNTT.simulateO48L6: no input provided"
      (packedBytes, validityRaw) = getSampleNTTOutput seed
      coeffs = unpackPython384Bytes packedBytes
      chunks = chunksOfO48 8 (assignCandidatesO48 validityRaw coeffs)
      readyPattern = expandBackpressureTiming backpressureTiming
      readyStream = case readyPattern of
        [] -> P.repeat P.True
        _ -> P.cycle readyPattern
      (idleOut, readyAfterIdle) = consumeN48 startSilence readyStream
      (permuteOut, readyAfterPermute) = consumeN48 25 readyAfterIdle
      (squeezeOut, _) = runO48Blocks chunks [] readyAfterPermute 0
   in compress48 (idleOut P.++ permuteOut P.++ squeezeOut)

runO48Blocks ::
  [[P.Maybe Word16]] ->
  [Word16] ->
  [P.Bool] ->
  P.Int ->
  ([P.Maybe (BitVector 48)], [P.Bool])
runO48Blocks chunks buffer rs emitted
  | emitted P.>= 64 = ([], rs)
  | P.otherwise =
      let (blockChunks, restChunks) = P.splitAt 14 chunks
       in if P.null blockChunks
            then
              if P.length buffer P.< 4
                then P.error "SampleNTT.simulateO48L6: candidate chunks exhausted"
                else
                  let (drainOut, buffer', rs', emitted') = runO48Drain buffer rs emitted
                   in if emitted' P.>= 64
                        then (drainOut, rs')
                        else runO48Blocks [] buffer' rs' emitted'
            else
              let (blockOut, buffer', rs', emitted') = runO48SqueezeBlock blockChunks buffer rs emitted
               in if emitted' P.>= 64
                    then (blockOut, rs')
                    else
                      let (gapOut, rs'') = consumeN48 24 rs'
                          (moreOut, rs''') = runO48Blocks restChunks buffer' rs'' emitted'
                       in (blockOut P.++ gapOut P.++ moreOut, rs''')

runO48SqueezeBlock ::
  [[P.Maybe Word16]] ->
  [Word16] ->
  [P.Bool] ->
  P.Int ->
  ([P.Maybe (BitVector 48)], [Word16], [P.Bool], P.Int)
runO48SqueezeBlock block buffer rs emitted = go 0 buffer rs emitted []
  where
    blockLen = P.length block
    go idx buf ready emitted' acc
      | emitted' P.>= 64 = (P.reverse acc, buf, ready, emitted')
      | idx P.>= blockLen = (P.reverse acc, buf, ready, emitted')
      | P.otherwise =
          case ready of
            [] -> P.error "SampleNTT.simulateO48L6: empty backpressure pattern"
            r : rs' ->
              let chunk = block P.!! idx
                  (outMaybe, buf', advanceIdx, produced) = stepO48 buf chunk r
                  idx' = if advanceIdx then idx P.+ 1 else idx
                  emitted'' = emitted' P.+ produced
               in go idx' buf' rs' emitted'' (outMaybe : acc)

stepO48 ::
  [Word16] ->
  [P.Maybe Word16] ->
  P.Bool ->
  (P.Maybe (BitVector 48), [Word16], P.Bool, P.Int)
stepO48 buffer chunk tready =
  case buffer of
    a : b : c : d : rest ->
      if tready
        then (P.Just (mkQuad a b c d), rest, P.False, 1)
        else (P.Nothing, buffer, P.False, 0)
    _ ->
      let vals = catMaybes chunk
          buffer' = buffer P.++ vals
       in case buffer' of
            a : b : c : d : rest ->
              if tready
                then (P.Just (mkQuad a b c d), rest, P.True, 1)
                else (P.Nothing, buffer', P.True, 0)
            _ -> (P.Nothing, buffer', P.True, 0)

runO48Drain ::
  [Word16] ->
  [P.Bool] ->
  P.Int ->
  ([P.Maybe (BitVector 48)], [Word16], [P.Bool], P.Int)
runO48Drain buffer rs emitted
  | emitted P.>= 64 = ([], buffer, rs, emitted)
  | P.length buffer P.< 4 = ([], buffer, rs, emitted)
  | P.otherwise =
      case rs of
        [] -> P.error "SampleNTT.simulateO48L6: empty backpressure pattern during drain"
        r : rs' ->
          let (outMaybe, buffer', produced) =
                case buffer of
                  a : b : c : d : rest ->
                    if r
                      then (P.Just (mkQuad a b c d), rest, 1)
                      else (P.Nothing, buffer, 0)
                  _ -> (P.Nothing, buffer, 0)
              (out, buffer'', rs'', emitted') = runO48Drain buffer' rs' (emitted P.+ produced)
           in (outMaybe : out, buffer'', rs'', emitted')

consumeN48 :: P.Int -> [P.Bool] -> ([P.Maybe (BitVector 48)], [P.Bool])
consumeN48 n rs =
  case n of
    0 -> ([], rs)
    _ ->
      case rs of
        _ : rs' ->
          let (out, rs'') = consumeN48 (n P.- 1) rs'
           in (Nothing : out, rs'')
        [] -> P.error "SampleNTT.simulateO48L6: empty backpressure pattern"

toBV12O48 :: Word16 -> BitVector 12
toBV12O48 v = P.fromIntegral v

mkQuad :: Word16 -> Word16 -> Word16 -> Word16 -> BitVector 48
mkQuad a b c d = toBV12O48 d ++# toBV12O48 c ++# toBV12O48 b ++# toBV12O48 a

chunksOfO48 :: P.Int -> [a] -> [[a]]
chunksOfO48 n xs =
  case P.splitAt n xs of
    ([], _) -> []
    (chunk, rest) -> chunk : chunksOfO48 n rest

assignCandidatesO48 :: [P.Bool] -> [Word16] -> [P.Maybe Word16]
assignCandidatesO48 [] [] = []
assignCandidatesO48 [] _ = P.error "SampleNTT.assignCandidatesO48: extra coefficients"
assignCandidatesO48 (v : vs) coeffs' =
  if v
    then case coeffs' of
      [] -> P.error "SampleNTT.assignCandidatesO48: ran out of coefficients"
      c : cs -> P.Just c : assignCandidatesO48 vs cs
    else P.Nothing : assignCandidatesO48 vs coeffs'

compress48 :: [P.Maybe (BitVector 48)] -> OutputTiming 48
compress48 [] = []
compress48 xs =
  case P.span (\v -> P.not (isJust v)) xs of
    (nothings, rest) | P.not (P.null nothings) ->
      Silent (P.length nothings) : compress48 rest
    _ ->
      let (justs, rest) = P.span isJust xs
          vals = [v | Just v <- justs]
       in Output vals : compress48 rest

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
          chunksPerBlock = (112 P.+ chunkWidth P.- 1) `P.div` chunkWidth
          blocks =
            if lookaheadCount P.== 4
              then buildL4Blocks candidates
              else
                let paddedBlockCandidates = chunksPerBlock P.* chunkWidth
                 in buildBufferedBlocks 112 paddedBlockCandidates chunkWidth candidates
          readyPattern = expandBackpressureTiming backpressureTiming
          readyStream = case readyPattern of
            [] -> P.repeat P.True
            _ -> P.cycle readyPattern
          (idleOut, readyAfterIdle) = consumeN startSilence readyStream
          (permuteOut, readyAfterPermute) = consumeN 25 readyAfterIdle
          drainDuringGap = lookaheadCount P.== 4
          emptyDrainBlock = P.replicate chunksPerBlock (P.replicate chunkWidth P.Nothing)
          (squeezeOut, _) = runBlocksBuffered drainDuringGap emptyDrainBlock blocks [] readyAfterPermute 0
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
      P.Bool ->
      [[P.Maybe Word16]] ->
      [[[P.Maybe Word16]]] ->
      [Word16] ->
      [P.Bool] ->
      P.Int ->
      ([P.Maybe (BitVector 24)], [P.Bool])
    runBlocksBuffered drain emptyDrainBlock blocks buffer rs emitted =
      if emitted P.>= 128
        then ([], rs)
        else case blocks of
          [] ->
            let (gapOut, buffer', rs', emitted') = runGapBuffered drain 24 buffer rs emitted
             in if emitted' P.>= 128
                  then (gapOut, rs')
                  else
                    if P.length buffer' P.< 2
                      then P.error "SampleNTT.simulateBuffered: candidate blocks exhausted"
                      else
                        let (drainOut, rs'') = runBlocksBuffered drain emptyDrainBlock [emptyDrainBlock] buffer' rs' emitted'
                         in (gapOut P.++ drainOut, rs'')
          block : rest ->
            let (blockOut, buffer', rs', emitted') = runBlockBuffered block buffer rs emitted
             in if emitted' P.>= 128
                  then (blockOut, rs')
                  else
                    let (gapOut, buffer'', rs'', emitted'') = runGapBuffered drain 24 buffer' rs' emitted'
                     in if emitted'' P.>= 128
                          then (blockOut P.++ gapOut, rs'')
                          else
                            let rest' = if P.null rest then [emptyDrainBlock] else rest
                                (moreOut, rs''') = runBlocksBuffered drain emptyDrainBlock rest' buffer'' rs'' emitted''
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
      P.Bool ->
      P.Int ->
      [Word16] ->
      [P.Bool] ->
      P.Int ->
      ([P.Maybe (BitVector 24)], [Word16], [P.Bool], P.Int)
    runGapBuffered drain n buffer rs emitted
      | n P.<= 0 = ([], buffer, rs, emitted)
      | emitted P.>= 128 = ([], buffer, rs, emitted)
      | P.otherwise =
          case rs of
            [] -> P.error "SampleNTT.simulateBuffered: empty backpressure pattern during permute gap"
            r : rs' ->
              if drain
                then
                  let (outMaybe, buffer', produced) =
                        case buffer of
                          a : b : rest ->
                            if r
                              then (P.Just (mkPair a b), rest, 1)
                              else (P.Nothing, buffer, 0)
                          _ -> (P.Nothing, buffer, 0)
                      (out, buffer'', rs'', emitted') =
                        runGapBuffered drain (n P.- 1) buffer' rs' (emitted P.+ produced)
                   in (outMaybe : out, buffer'', rs'', emitted')
                else
                  let (out, buffer', rs'', emitted') =
                        runGapBuffered drain (n P.- 1) buffer rs' emitted
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
