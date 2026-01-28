{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTT2
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.SampleNTT2 qualified as SampleNTT2
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word16)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Test.Hspec (Expectation, shouldBe)
import Test.TestHarness.ExternalReference (callPythonReference)
import Test.TestHarness.SHAKECommon
  ( DownstreamBackpressure (..),
    ShakeTest (..),
    UpstreamStall (..),
    testLabel,
  )
import Prelude qualified as P

--------------------------------------------------------------------------------
-- SampleNTT2-specific types
--------------------------------------------------------------------------------

type SampleNTT2TopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)

data SampleNTT2Params = SampleNTT2Params
  { spReference :: ByteString -> [Word16],
    spTopEntity :: SampleNTT2TopEntity
  }

--------------------------------------------------------------------------------
-- Default harness configuration
--------------------------------------------------------------------------------

sampleNTT2Params :: SampleNTT2Params
sampleNTT2Params =
  SampleNTT2Params
    { spReference = unpackPython384Bytes . externalSampleNTTPacked,
      spTopEntity = SampleNTT2.topEntity
    }

-- | External reference implementation using kyber-py
externalSampleNTTPacked :: ByteString -> ByteString
externalSampleNTTPacked = callPythonReference ("reference" </> "kyber" </> "sample_ntt.py")

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

runTest :: ShakeTest -> Expectation
runTest = runSampleNTT2Test sampleNTT2Params

runHardware :: ShakeTest -> [Word16]
runHardware = runSampleNTT2Hardware sampleNTT2Params

runSampleNTT2Test :: SampleNTT2Params -> ShakeTest -> Expectation
runSampleNTT2Test params test = do
  let expected = P.map reverseBits12Word (spReference params (testMessage test))
      actual = runSampleNTT2Hardware params test
  P.length actual `shouldBe` 256
  P.length expected `shouldBe` 256
  actual `shouldBe` expected

runSampleNTT2Hardware :: SampleNTT2Params -> ShakeTest -> [Word16]
runSampleNTT2Hardware params test =
  let msgBV = bsToBV272 (testMessage test)
      treadyPattern = backpressurePattern (testDownstreamBackpressure test)
      treadySignal = makeBackpressureSignalRepeat treadyPattern
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          inputSig
      msgStreamSig = fmap (\v -> AXI4Stream msgBV v False) msgValidSig
      inputSig = bundle (msgStreamSig, treadySignal)
      msgReadySig = fmap snd output
      (msgValidSig, msgReadyPrevSig) =
        withClockResetEnable clockGen resetGen enableGen $
          let msgReadyPrevSig' = register True msgReadySig
              msgValidSig' = makeUpstreamValidSignal (testUpstreamStall test) msgReadyPrevSig'
           in (msgValidSig', msgReadyPrevSig')
      validityPattern = getTimingInfo (testMessage test)
      sampleCount =
        simulateTiming2
          validityPattern
          (testUpstreamStall test)
          (testDownstreamBackpressure test)
          P.+ 5
      samples = sampleN @System sampleCount (bundle (output, treadySignal))
      validOutputs =
        [ tdata stream
          | ((stream, _), ready) <- samples,
            tvalid stream P.&& ready
        ]
      coeffPairs = P.map splitCoeffs24 validOutputs
      coeffs = P.take 256 (P.concatMap (\(c0, c1) -> [c0, c1]) coeffPairs)
      readySamples = sampleN @System sampleCount (bundle (msgValidSig, msgReadyPrevSig))
      readyViolations = [() | (valid, readyPrev) <- readySamples, valid P.&& P.not readyPrev]
      handshakes = [() | (valid, readyPrev) <- readySamples, valid P.&& readyPrev]
   in if P.null readyViolations
        then
          if P.null handshakes
            then P.error "SampleNTT2: MSG_TVALID never asserted after MSG_TREADY"
            else coeffs
        else P.error "SampleNTT2: MSG_TVALID asserted without MSG_TREADY in previous cycle"

-- | Generate MSG_TVALID signal based on upstream stall pattern.
-- The signal is asserted once, after MSG_TREADY was observed high and stalls are done.
makeUpstreamValidSignal :: (HiddenClockResetEnable dom) => UpstreamStall -> Signal dom Bool -> Signal dom Bool
makeUpstreamValidSignal control readyPrevSig =
  mealy step (False, stallPattern) readyPrevSig
  where
    stallPattern = case control of
      NoUpstreamStall -> []
      UpstreamStall xs -> xs
    step (sent, pattern) readyPrev =
      if sent
        then ((sent, pattern), False)
        else
          let (stallNow, pattern') = case pattern of
                [] -> (False, [])
                b : bs -> (b, bs)
              canSend = readyPrev P.&& P.not stallNow
           in if canSend
                then ((True, pattern'), True)
                else ((False, pattern'), False)

makeBackpressureSignalRepeat :: [Bool] -> Signal System Bool
makeBackpressureSignalRepeat pattern =
  fromList (P.cycle pattern)

backpressurePattern :: DownstreamBackpressure -> [Bool]
backpressurePattern NoDownstreamBackpressure = [True]
backpressurePattern (DownstreamBackpressure pattern) =
  let hasReady = P.any P.id pattern
   in if hasReady then pattern else pattern P.++ [True]

splitCoeffs24 :: BitVector 24 -> (Word16, Word16)
splitCoeffs24 bv =
  let coeff0 = unpack (slice (SNat @11) (SNat @0) bv) :: Unsigned 12
      coeff1 = unpack (slice (SNat @23) (SNat @12) bv) :: Unsigned 12
   in (P.fromIntegral coeff0, P.fromIntegral coeff1)

bsToBV272 :: ByteString -> BitVector 272
bsToBV272 bs =
  let padded = BS.take 34 (bs P.<> BS.replicate 34 0)
      bytes = BS.unpack padded
      -- Bit-reverse each byte to match the Reversed permutation's expectations
      step acc w = (acc `shiftL` 8) .|. resize (reverseBits8 (pack (fromIntegral w :: BitVector 8)))
   in P.foldl step (0 :: BitVector 272) bytes

reverseBits8 :: BitVector 8 -> BitVector 8
reverseBits8 bv = pack (reverse (unpack bv :: Vec 8 Bit))

reverseBits12Word :: Word16 -> Word16
reverseBits12Word w =
  let bv = pack (fromIntegral w :: Unsigned 12)
      rev = pack (reverse (unpack bv :: Vec 12 Bit))
   in P.fromIntegral (unpack rev :: Unsigned 12)

--------------------------------------------------------------------------------
-- Unpacking Python 384-byte format
--------------------------------------------------------------------------------

unpackPython384Bytes :: ByteString -> [Word16]
unpackPython384Bytes bs = go (BS.unpack bs)
  where
    go (b0 : b1 : b2 : rest) =
      let c0 = P.fromIntegral b0 P.+ 256 P.* (P.fromIntegral b1 .&. 0x0F)
          c1 = (P.fromIntegral b1 `shiftR` 4) P.+ 16 P.* P.fromIntegral b2
       in c0 : c1 : go rest
    go [] = []
    go _ = P.error "unpackPython384Bytes: Expected 384 bytes (multiple of 3)"

--------------------------------------------------------------------------------
-- Timing functions for exact cycle count verification
--------------------------------------------------------------------------------

getTimingInfo :: ByteString -> [Bool]
getTimingInfo input = unsafePerformIO $ do
  (Just hIn, Just hOut, _, ph) <-
    createProcess
      (proc "python3" ["reference/kyber/sample_ntt_timing.py"])
        { std_in = CreatePipe,
          std_out = CreatePipe
        }
  BS.hPut hIn input
  hClose hIn
  output <- LBS.hGetContents hOut
  _ <- waitForProcess ph
  case eitherDecode output of
    Left err -> P.error $ "Failed to parse validity pattern: " P.++ err
    Right vp -> P.return vp
{-# NOINLINE getTimingInfo #-}

simulateTiming2 ::
  [Bool] ->
  UpstreamStall ->
  DownstreamBackpressure ->
  Int
simulateTiming2 validityPattern upstreamStall backpressure =
  let upstreamStallCycles = case upstreamStall of
        NoUpstreamStall -> 0
        UpstreamStall pattern -> P.length (P.takeWhile P.id pattern)
      bpPattern = P.cycle (backpressurePattern backpressure)
      simulateSqueeze ::
        Int ->
        Bool ->
        [Bool] ->
        [Bool] ->
        (Int, Int, Bool, [Bool], [Bool])
      simulateSqueeze = go (0 :: Int) (0 :: Int)
        where
          go cycles squeezeIdx validCnt buf val bpPat
            | validCnt P.>= 256 = (cycles, validCnt, buf, bpPat, val)
            | squeezeIdx P.>= 112 = (cycles, validCnt, buf, bpPat, val)
            | P.otherwise =
                let (tready, bpPat') = case bpPat of
                      [] -> (True, [])
                      (b : bs) -> (b, bs)
                 in if P.not tready
                      then go (cycles P.+ 1) squeezeIdx validCnt buf val bpPat'
                      else case val of
                        [] -> P.error "simulateTiming2: validity pattern exhausted"
                        (v : vs) ->
                          let (buf', emitted) =
                                if buf
                                  then if v then (False, 2) else (True, 0)
                                  else if v then (True, 0) else (False, 0)
                              validCnt' = validCnt P.+ emitted
                           in go (cycles P.+ 1) (squeezeIdx P.+ 1) validCnt' buf' vs bpPat'
      simulateBlocks ::
        Int ->
        Int ->
        Bool ->
        [Bool] ->
        [Bool] ->
        Int
      simulateBlocks totalCycles validCount buf validity bp
        | validCount P.>= 256 = totalCycles
        | P.otherwise =
            let afterPermute = totalCycles P.+ 24
                (squeezeCycles, newValidCount, newBuf, newBp, newValidity) =
                  simulateSqueeze validCount buf validity bp
             in simulateBlocks (afterPermute P.+ squeezeCycles) newValidCount newBuf newValidity newBp
      initialCycles = 1 P.+ upstreamStallCycles P.+ 1
      bpAfterInit = P.drop initialCycles bpPattern
   in simulateBlocks initialCycles 0 False validityPattern bpAfterInit
