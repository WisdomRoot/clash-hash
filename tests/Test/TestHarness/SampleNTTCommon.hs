{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTTCommon
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
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word16)
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Test.Hspec (Expectation, shouldBe)
import Test.TestHarness.SHAKECommon (DownstreamBackpressure (..), ShakeTest (..), UpstreamStall (..), makeBackpressureTest, makeBasicTest, makeCombinedTest, makeStallTest, makeVariableOutputTest, testLabel)
import Test.TestHarness.StreamCommon (makeBackpressureSignal)
import Prelude qualified as P

--------------------------------------------------------------------------------
-- SampleNTT-specific types
--------------------------------------------------------------------------------

type SampleNTTTopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 272) ->
  Signal System Bool ->
  Signal System Bool ->
  Signal System (Bool, AXI4Stream 12)

data SampleNTTParams = SampleNTTParams
  { spReference :: ByteString -> [Word16],
    spTopEntity :: SampleNTTTopEntity
  }

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

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
      msgDataSig = pure msgBV
      -- MSG_TVALID is True from the start (can be delayed with upstream stall pattern)
      msgValidSig = makeUpstreamValidSignal (testUpstreamStall test)
      treadySignal = makeBackpressureSignal (testDownstreamBackpressure test)
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          msgDataSig
          msgValidSig
          treadySignal
      -- Get exact timing from Python reference
      validityPattern = getTimingInfo (testMessage test)
      sampleCount =
        simulateTiming
          validityPattern
          (testUpstreamStall test)
          (testDownstreamBackpressure test)
      samples = sampleN @System sampleCount (bundle (output, treadySignal))
      validOutputs =
        [ tdata stream
          | ((_, stream), ready) <- samples,
            tvalid stream P.&& ready
        ]
      coeffs = P.map (P.fromIntegral . (unpack :: BitVector 12 -> Unsigned 12)) (P.take 256 validOutputs)
   in coeffs

-- | Generate MSG_TVALID signal based on upstream stall pattern
-- NoUpstreamStall: MSG_TVALID is True from the start
-- UpstreamStall pattern: MSG_TVALID is False during stall (True in pattern), then True forever
makeUpstreamValidSignal :: UpstreamStall -> Signal System Bool
makeUpstreamValidSignal NoUpstreamStall = pure True
makeUpstreamValidSignal (UpstreamStall pattern) =
  -- Pattern: True means stall (don't send valid), False means ready to send
  -- After pattern is exhausted, default to True (always valid)
  fromList (P.map P.not pattern P.++ P.repeat True)

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
      -- Note: The first element of the pattern corresponds to the reset cycle,
      -- which is wasted (state can't transition during reset). So we skip it.
      -- Pattern semantics: True = stall (msgValid=False), False = ready (msgValid=True)
      upstreamStallCycles = case upstreamStall of
        NoUpstreamStall -> 0
        UpstreamStall pattern -> P.length (P.takeWhile P.id (P.drop 1 pattern))

      -- Backpressure pattern (True = ready, False = not ready)
      bpPattern = case backpressure of
        NoDownstreamBackpressure -> P.repeat True
        DownstreamBackpressure pattern -> pattern P.++ P.repeat True

      -- Simulate squeeze phases
      -- Returns: (cycles, validCount, remainingBpPattern, remainingValidityPattern)
      simulateSqueeze ::
        Int -> -- valid count so far
        [Bool] -> -- remaining validity pattern
        [Bool] -> -- remaining backpressure pattern
        (Int, Int, [Bool], [Bool]) -- (cycles, validCount, bpPattern, validityPattern)
      simulateSqueeze  = go (0 :: Int) (0 :: Int)
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
   in simulateBlocks initialCycles 0 validityPattern bpPattern
