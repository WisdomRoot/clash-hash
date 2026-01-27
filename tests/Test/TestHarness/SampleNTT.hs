{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTT
  ( -- Re-export shared types from SHAKECommon
    UpstreamStall (..),
    DownstreamBackpressure (..),
    ShakeTest (..),
    testLabel,
    makeBasicTest,
    makeVariableOutputTest,
    makeStallTest,
    makeBackpressureTest,
    makeCombinedTest,
    -- SampleNTT-specific types and functions
    SampleNTTParams (..),
    SampleNTTTopEntity,
    runSampleNTTTest,
    runSampleNTTHardware,
    unpackPython384Bytes,
    -- Timing functions
    getTimingInfo,
    simulateTiming,
    -- Default harness entry points
    runTest,
    runHardware,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.SampleNTT qualified as SampleNTT
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
    makeBackpressureTest,
    makeBasicTest,
    makeCombinedTest,
    makeStallTest,
    makeVariableOutputTest,
    testLabel,
  )
import Prelude qualified as P

--------------------------------------------------------------------------------
-- SampleNTT-specific types
--------------------------------------------------------------------------------

type SampleNTTTopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 12, Bool)

data SampleNTTParams = SampleNTTParams
  { spReference :: ByteString -> [Word16],
    spTopEntity :: SampleNTTTopEntity
  }

--------------------------------------------------------------------------------
-- Default harness configuration
--------------------------------------------------------------------------------

sampleNTTParams :: SampleNTTParams
sampleNTTParams =
  SampleNTTParams
    { spReference = unpackPython384Bytes . externalSampleNTTPacked,
      spTopEntity = SampleNTT.topEntity
    }

-- | External reference implementation using kyber-py
-- Returns 384 packed bytes (256 coefficients × 12 bits packed as triplets)
externalSampleNTTPacked :: ByteString -> ByteString
externalSampleNTTPacked = callPythonReference ("reference" </> "kyber" </> "sample_ntt.py")

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

runTest :: ShakeTest -> Expectation
runTest = runSampleNTTTest sampleNTTParams

runHardware :: ShakeTest -> [Word16]
runHardware = runSampleNTTHardware sampleNTTParams

runSampleNTTTest :: SampleNTTParams -> ShakeTest -> Expectation
runSampleNTTTest params test = do
  let expected = P.map reverseBits12Word (spReference params (testMessage test))
      actual = runSampleNTTHardware params test
  P.length actual `shouldBe` 256
  P.length expected `shouldBe` 256
  actual `shouldBe` expected

runSampleNTTHardware :: SampleNTTParams -> ShakeTest -> [Word16]
runSampleNTTHardware params test =
  let msgBV = bsToBV272 (testMessage test)
      treadyPattern = backpressurePattern (testDownstreamBackpressure test)
      treadySignal = makeBackpressureSignalRepeat treadyPattern
      -- Build input AXI4Stream signal from msgBV and msgValidSig
      msgStreamSig = fmap (\v -> AXI4Stream msgBV v False) msgValidSig
      inputSig = bundle (msgStreamSig, treadySignal)
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          inputSig
      msgReadySig = fmap snd output
      (msgValidSig, msgReadyPrevSig) =
        withClockResetEnable clockGen resetGen enableGen $
          let msgReadyPrevSig' = register True msgReadySig
              msgValidSig' = makeUpstreamValidSignal (testUpstreamStall test) msgReadyPrevSig'
           in (msgValidSig', msgReadyPrevSig')
      -- Get exact timing from Python reference
      validityPattern = getTimingInfo (testMessage test)
      sampleCount =
        simulateTiming
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
      coeffs = P.map (P.fromIntegral . (unpack :: BitVector 12 -> Unsigned 12)) (P.take 256 validOutputs)
      readySamples = sampleN @System sampleCount (bundle (msgValidSig, msgReadyPrevSig))
      readyViolations = [() | (valid, readyPrev) <- readySamples, valid P.&& P.not readyPrev]
      handshakes = [() | (valid, readyPrev) <- readySamples, valid P.&& readyPrev]
   in if P.null readyViolations
        then
          if P.null handshakes
            then P.error "SampleNTT: MSG_TVALID never asserted after MSG_TREADY"
            else coeffs
        else P.error "SampleNTT: MSG_TVALID asserted without MSG_TREADY in previous cycle"

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

-- | Unpack Python's 384-byte format to 256 coefficients
-- Python packs two 12-bit coefficients into 3 bytes (128 triplets):
--   c0 = byte0 + 256 * (byte1 & 0x0F)  (bits 0-11)
--   c1 = (byte1 >> 4) + 16 * byte2      (bits 12-23)
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

-- | Get validity pattern from Python reference script
-- Returns which coefficients pass rejection sampling (< 3329)
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

-- | Simulate exact timing based on validity pattern and stall/backpressure
-- Returns the exact number of cycles needed to collect 256 valid coefficients
simulateTiming ::
  [Bool] -> -- validity pattern from Python
  UpstreamStall ->
  DownstreamBackpressure ->
  Int -- exact cycle count
simulateTiming validityPattern upstreamStall backpressure =
  let -- Count upstream stall cycles
      -- Pattern semantics: True = stall (msgValid=False), False = ready (msgValid=True)
      upstreamStallCycles = case upstreamStall of
        NoUpstreamStall -> 0
        UpstreamStall pattern -> P.length (P.takeWhile P.id pattern)

      -- Backpressure pattern (True = ready, False = not ready)
      bpPattern = P.cycle (backpressurePattern backpressure)

      -- Simulate squeeze phases
      -- Returns: (cycles, validCount, remainingBpPattern, remainingValidityPattern)
      simulateSqueeze ::
        Int -> -- valid count so far
        [Bool] -> -- remaining validity pattern
        [Bool] -> -- remaining backpressure pattern
        (Int, Int, [Bool], [Bool]) -- (cycles, validCount, bpPattern, validityPattern)
      simulateSqueeze = go (0 :: Int) (0 :: Int)
        where
          go cycles squeezeIdx validCnt val bpPat
            -- Reached 256 valid coefficients - done
            | validCnt P.>= 256 = (cycles, validCnt, bpPat, val)
            -- Exhausted 112 coefficients in this squeeze block - need another permute
            | squeezeIdx P.>= 112 = (cycles, validCnt, bpPat, val)
            | P.otherwise =
                let (tready, bpPat') = case bpPat of
                      [] -> (True, [])
                      (b : bs) -> (b, bs)
                 in if tready
                      then
                        -- Advance: capture coefficient if valid, move to next
                        let (isValid, val') = case val of
                              [] -> P.error "simulateTiming: validity pattern exhausted"
                              (v : vs) -> (v, vs)
                            validCnt' = if isValid then validCnt P.+ 1 else validCnt
                         in go (cycles P.+ 1) (squeezeIdx P.+ 1) validCnt' val' bpPat'
                      else
                        -- Stall: stay at same coefficient, consume backpressure cycle
                        go (cycles P.+ 1) squeezeIdx validCnt val bpPat'

      -- Simulate full operation: permute blocks + squeeze blocks until 256 valid
      simulateBlocks ::
        Int -> -- total cycles so far
        Int -> -- valid count so far
        [Bool] -> -- remaining validity pattern
        [Bool] -> -- remaining backpressure pattern
        Int -- final cycle count
      simulateBlocks totalCycles validCount validity bp
        | validCount P.>= 256 = totalCycles
        | P.otherwise =
            let -- Permute phase: 24 cycles
                afterPermute = totalCycles P.+ 24
                -- Squeeze phase: simulate until 112 coefficients or 256 valid
                (squeezeCycles, newValidCount, newBp, newValidity) =
                  simulateSqueeze validCount validity bp
             in simulateBlocks (afterPermute P.+ squeezeCycles) newValidCount newValidity newBp

      -- Start simulation:
      -- 1. Reset cycle (Clash resetGen is active for first cycle)
      -- 2. Upstream stall cycles (waiting for msgValid)
      -- 3. Idle→Permute transition (1 cycle when msgValid becomes True)
      -- 4. Permute/Squeeze blocks until 256 valid
      initialCycles = 1 P.+ upstreamStallCycles P.+ 1 -- reset + stalls + transition
      bpAfterInit = P.drop initialCycles bpPattern
   in simulateBlocks initialCycles 0 validityPattern bpAfterInit
