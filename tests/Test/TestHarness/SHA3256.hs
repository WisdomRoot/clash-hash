{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Self-contained test harness infrastructure for SHA3-256 hardware testing
--
-- HARDWARE CONSTRAINTS:
-- - Input lengths must be multiples of 64 bits (8 bytes)
-- - Output is fixed at 256 bits (32 bytes)
--
-- Does not depend on Test.TestCase module
module Test.TestHarness.SHA3256
  ( -- * Test data types
    SHA3256Test (..),
    UpstreamStall (..),
    DownstreamBackpressure (..),

    -- * Test execution
    runTest,
    testLabel,

    -- * Common test messages
    msg1024,
    msg1088,
    msg1600,
    msg3200,

    -- * Stall patterns
    stallPatternSimple,
    stallPatternModerate,
    stallPatternAggressive,

    -- * Backpressure patterns
    backpressurePatternSimple,
    backpressurePatternModerate,
    backpressurePatternAggressive,

    -- * Test case builders
    makeBasicTest,
    makeStallTest,
    makeBackpressureTest,
    makeCombinedTest,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Clash.Sized.Vector qualified as V
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Hash.NonPipelined.SHA3256 qualified as SHA3256
import Prelude qualified as P
import Reference.Hash qualified as Hash
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck (Arbitrary (..), frequency, listOf1, vector)

--------------------------------------------------------------------------------
-- Test data types
--------------------------------------------------------------------------------

-- | Upstream stall control
data UpstreamStall
  = NoUpstreamStall
  | UpstreamStall [Bool]
  deriving (Show)

-- | Downstream backpressure control
data DownstreamBackpressure
  = NoDownstreamBackpressure
  | DownstreamBackpressure [Bool]
  deriving (Show)

-- | A SHA3-256 test case (fixed 32-byte output)
data SHA3256Test = SHA3256Test
  { testMessage :: ByteString,
    testUpstreamStall :: UpstreamStall,
    testDownstreamBackpressure :: DownstreamBackpressure
  }
  deriving (Show)

-- | Generate a test label
testLabel :: SHA3256Test -> String
testLabel test =
  show inputBits
    <> "-bit input"
    <> stallInfo
    <> backpressureInfo
  where
    inputBits = BS.length (testMessage test) P.* 8
    stallInfo = case testUpstreamStall test of
      NoUpstreamStall -> ""
      UpstreamStall _ -> " [with stalls]"
    backpressureInfo = case testDownstreamBackpressure test of
      NoDownstreamBackpressure -> ""
      DownstreamBackpressure _ -> " [with backpressure]"

--------------------------------------------------------------------------------
-- QuickCheck Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary UpstreamStall where
  arbitrary =
    frequency
      [ (3, pure NoUpstreamStall), -- 75% no stalls
        (1, UpstreamStall P.<$> genStalls) -- 25% random stalls
      ]
    where
      genStalls = listOf1 (frequency [(3, pure True), (1, pure False)])

instance Arbitrary DownstreamBackpressure where
  arbitrary =
    frequency
      [ (3, pure NoDownstreamBackpressure), -- 75% no backpressure
        (1, DownstreamBackpressure P.<$> genBackpressure) -- 25% random backpressure
      ]
    where
      genBackpressure = listOf1 (frequency [(3, pure True), (1, pure False)])

instance Arbitrary SHA3256Test where
  arbitrary = do
    -- IMPORTANT: All sizes are in beats (1 beat = 8 bytes = 64 bits)
    -- Hardware spec requires input lengths to be multiples of 64 bits
    beatCount <-
      frequency
        [ (2, pure 1), -- 8 bytes - emphasize minimum
          (1, pure 2), -- 16 bytes
          (1, pure 16), -- 128 bytes - just under one block
          (2, pure 17), -- 136 bytes - exact block boundary (1088 bits) - important!
          (1, pure 18), -- 144 bytes - just over one block
          (2, pure 25), -- 200 bytes - full Keccak state (1600 bits) - important!
          (1, pure 34), -- 272 bytes - two blocks
          (1, pure 51) -- 408 bytes - three blocks
        ]
    -- Generate random bytes (8 bytes per beat)
    messageBytes <- BS.pack P.<$> vector (beatCount P.* 8)
    upstreamStall <- arbitrary
    SHA3256Test messageBytes upstreamStall <$> arbitrary

--------------------------------------------------------------------------------
-- Test execution
--------------------------------------------------------------------------------

-- | Run a SHA3-256 hardware test against the reference implementation
runTest :: SHA3256Test -> Expectation
runTest test = do
  let inputBS = testMessage test

      -- Get expected result from reference implementation (SHA3-256 always outputs 32 bytes)
      expectedBS = Hash.sha3_256BS inputBS

      -- Run hardware simulation
      actualBS = runHardware test

  -- Compare results
  actualBS `shouldBe` expectedBS

-- | Run SHA3-256 hardware simulation
runHardware :: SHA3256Test -> ByteString
runHardware test =
  let inputBS = testMessage test
      inputBytes = BS.length inputBS

      -- Calculate number of 64-bit beats needed (round up)
      beats = (inputBytes P.+ 7) `P.div` 8
   in case fromJust (someNatVal (P.fromIntegral beats)) of
        SomeNat (_ :: Proxy beats') ->
          runHardware' @beats' test beats

-- | Run hardware with KnownNat constraint available
runHardware' ::
  forall beats.
  KnownNat beats =>
  SHA3256Test ->
  Int ->
  ByteString
runHardware' test beats =
  let inputBS = testMessage test
      inputBits = bsToBitListHW inputBS

      -- Hardware applies domain separator internally via padSHA3
      -- So we don't add it here

      -- Pad input to multiple of 64 bits
      paddedBits = P.take (beats P.* 64) (inputBits P.++ P.repeat 0)

      -- Convert to Vec of 64-bit words
      messageWords = bitListToWords @beats beats paddedBits

      -- Create input stream with stall pattern (returns both stream and flush signal)
      inputStreamRaw =
        withClockResetEnable clockGen resetGen enableGen $
          feedInput @beats (testUpstreamStall test) messageWords

      -- Create downstream ready signal with backpressure pattern
      treadySignal = makeBackpressureSignal (testDownstreamBackpressure test)

      -- Bundle input stream with flush signal for topEntity
      inputStream = inputStreamRaw

      -- Run through hardware
      output =
        SHA3256.topEntity
          clockGen
          resetGen
          enableGen
          treadySignal
          inputStream

      -- SHA3-256 always outputs exactly 4 beats (256 bits = 32 bytes)
      outputBytes = 32
      outputBits = outputBytes P.* 8
      outputBeats = 4

      -- Conservative estimate for sampling
      sampleCount = beats P.* 2 P.+ 24 P.+ outputBeats P.+ 24 P.+ 200

      samples = sampleN @System sampleCount output

      -- Extract valid output data
      validOutputs = [tdata stream | (stream, _) <- samples, tvalid stream]

      -- Convert output words to bits and take required amount
      -- Take exactly outputBeats (4) before converting to bits
      outputWordBits = P.concatMap wordToBits (P.take outputBeats validOutputs)
      resultBits = P.take outputBits outputWordBits

   in bitListToBSHW resultBits

-- | Feed input message to hardware with optional stall pattern
feedInput ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  UpstreamStall ->
  Vec beats (BitVector 64) ->
  Signal dom (AXI4Stream 64, Bool)
feedInput control messageWords =
  let isEmpty = V.length messageWords == 0
   in mealy step (toList messageWords, 0 :: Int, 0 :: Int, stallPattern, isEmpty) (pure ())
  where
    stallPattern = case control of
      NoUpstreamStall -> []
      UpstreamStall xs -> xs

    beatsPerBlock = 17 :: Int  -- Rate 1088 bits / 64 bits per beat = 17 beats
    permuteLatency = 24 :: Int  -- Keccak-f[1600] = 24 rounds

    step (xs, waitCount, emittedInBlock, ctrl, wasEmpty) _ =
      if waitCount P.> 0
        then ((xs, waitCount P.- 1, emittedInBlock, ctrl, wasEmpty), (idleBeat, False))
        else
          let (canSend, ctrl') = case ctrl of
                [] -> (True, [])
                b : bs -> (b, bs)
           in if P.not canSend
                then ((xs, waitCount, emittedInBlock, ctrl', wasEmpty), (idleBeat, False))
                else case xs of
                  -- Empty input not supported for SHA3-256
                  [] | wasEmpty ->
                       P.error "SHA3-256 does not support empty input"
                  -- After completion
                  [] -> (([], 0, 0, ctrl', False), (idleBeat, False))
                  -- Normal data beats
                  y : ys ->
                    let isLast = P.null ys
                        emittedNow = emittedInBlock P.+ 1
                        blockCompleted = emittedNow == beatsPerBlock
                        needGap = blockCompleted P.&& P.not isLast
                        nextWait = if needGap then permuteLatency else 0
                        nextEmitted = if blockCompleted then 0 else emittedNow
                        nextState = (ys, nextWait, nextEmitted, ctrl', False)
                        outBeat =
                          AXI4Stream
                            { tdata = y,
                              tvalid = True,
                              tlast = isLast
                            }
                     in (nextState, (outBeat, False))

    idleBeat =
      AXI4Stream
        { tdata = 0,
          tvalid = False,
          tlast = False
        }

-- | Create backpressure signal pattern
makeBackpressureSignal :: DownstreamBackpressure -> Signal System Bool
makeBackpressureSignal NoDownstreamBackpressure = pure True
makeBackpressureSignal (DownstreamBackpressure pattern) =
  fromList (pattern P.++ P.repeat True)

-- | Convert ByteString to bit list (LSB-first per byte, matching working SHA3 tests)
bsToBitListHW :: ByteString -> [Bit]
bsToBitListHW bs =
  P.concatMap word8ToBits (BS.unpack bs)
  where
    word8ToBits :: Word8 -> [Bit]
    word8ToBits w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 7]]

-- | Convert bit list to ByteString (reverse of bsToBitListHW, matching working SHA3 tests)
bitListToBSHW :: [Bit] -> ByteString
bitListToBSHW bits =
  BS.pack (packBytes bits)
  where
    packBytes :: [Bit] -> [Word8]
    packBytes [] = []
    packBytes bs =
      let (chunk, rest) = P.splitAt 8 bs
          byte = P.foldl setBit' 0 (P.zip [0 ..] chunk)
       in byte : packBytes rest
    setBit' :: Word8 -> (Int, Bit) -> Word8
    setBit' acc (i, b) = if b P.== 1 then Bits.setBit acc i else acc

-- | Convert list of bits to Vec of 64-bit words
--   Hardware XORs beat 0 into bits 1599:1536, beat 1 into 1535:1472, etc.
--   This means bits are indexed from MSB to LSB
bitListToWords :: forall beats. KnownNat beats => Int -> [Bit] -> Vec beats (BitVector 64)
bitListToWords n bits =
  let chunks = chunksOf 64 bits
      wordsList = P.map bitsToWord (P.take n chunks)
   in V.unsafeFromList wordsList
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf m xs = P.take m xs : chunksOf m (P.drop m xs)

    bitsToWord :: [Bit] -> BitVector 64
    bitsToWord bs =
      let paddedBits = P.take 64 (bs P.++ P.repeat 0)
          -- Reverse bits: first bit in list should go to MSB (bit 63) of word
          -- because hardware XORs block bit 63 into state bit 1599
          word = P.foldl accumBit 0 (P.zip [63, 62 .. 0] paddedBits)
       in word
      where
        accumBit :: BitVector 64 -> (Int, Bit) -> BitVector 64
        accumBit acc (i, b) = if b P.== 1 then Bits.setBit acc i else acc

-- | Convert 64-bit word to list of bits (MSB first)
wordToBits :: BitVector 64 -> [Bit]
wordToBits w = [if Bits.testBit w i then 1 else 0 | i <- [63, 62 .. 0]]

--------------------------------------------------------------------------------
-- Common test messages
--------------------------------------------------------------------------------

-- IMPORTANT: All test message inputs MUST be multiples of 8 bytes (64 bits)
-- due to hardware limitation in domain separator placement. The hardware
-- assumes complete 64-bit beats and places the SHA3-256 domain separator
-- (2 bits: 01) at fixed positions based on beat counter, not actual
-- message bit length.

-- | 128 bytes = 1024 bits (fits in one rate block, rate=1088)
msg1024 :: ByteString
msg1024 =
  "qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh\
  \qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh"

-- | 136 bytes = 1088 bits (exactly one rate block)
msg1088 :: ByteString
msg1088 =
  "qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh\
  \qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh\
  \qwertyui"

-- | 200 bytes = 1600 bits (full Keccak state)
msg1600 :: ByteString
msg1600 =
  "01234567890123456789012345678901234567890123456789\
  \01234567890123456789012345678901234567890123456789\
  \01234567890123456789012345678901234567890123456789\
  \01234567890123456789012345678901234567890123456789"

-- | 400 bytes = 3200 bits (multiple blocks)
msg3200 :: ByteString
msg3200 =
  "01234567890123456789012345678901234567890123456789\
  \01234567890123456789012345678901234567890123456789\
  \01234567890123456789012345678901234567890123456789\
  \01234567890123456789012345678901234567890123456789\
  \01234567890123456789012345678901234567890123456789\
  \01234567890123456789012345678901234567890123456789\
  \01234567890123456789012345678901234567890123456789\
  \01234567890123456789012345678901234567890123456789"

--------------------------------------------------------------------------------
-- Upstream stall patterns
--------------------------------------------------------------------------------

-- | Simple stall pattern: mostly sends with occasional stalls (~20% stalls)
stallPatternSimple :: [Bool]
stallPatternSimple =
  [True, True, False, True, True, True, False, True, True, True]

-- | Moderate stall pattern: 50% stalls
stallPatternModerate :: [Bool]
stallPatternModerate =
  [True, False, True, False, True, False, True, False, True, False]

-- | Aggressive stall pattern: frequent stalls (~70% stalls)
stallPatternAggressive :: [Bool]
stallPatternAggressive =
  [True, False, False, True, False, True, False, False, False, True]

--------------------------------------------------------------------------------
-- Downstream backpressure patterns
--------------------------------------------------------------------------------

-- | Simple backpressure: mostly accepts with occasional pauses (~20% not ready)
backpressurePatternSimple :: [Bool]
backpressurePatternSimple =
  [True, True, True, False, True, True, True, False, True, True]

-- | Moderate backpressure: 50% ready
backpressurePatternModerate :: [Bool]
backpressurePatternModerate =
  [True, False, True, False, True, False, True, False, True, False]

-- | Aggressive backpressure: frequent not-ready (~70% not ready)
backpressurePatternAggressive :: [Bool]
backpressurePatternAggressive =
  [True, False, False, False, True, False, False, True, False, False]

--------------------------------------------------------------------------------
-- Test case builder functions
--------------------------------------------------------------------------------

-- | Create a basic SHA3-256 test with no stalls or backpressure
makeBasicTest :: ByteString -> SHA3256Test
makeBasicTest input =
  SHA3256Test input NoUpstreamStall NoDownstreamBackpressure

-- | Create a test with upstream stalls
makeStallTest :: ByteString -> [Bool] -> SHA3256Test
makeStallTest input pattern =
  SHA3256Test input (UpstreamStall pattern) NoDownstreamBackpressure

-- | Create a test with downstream backpressure
makeBackpressureTest :: ByteString -> [Bool] -> SHA3256Test
makeBackpressureTest input pattern =
  SHA3256Test input NoUpstreamStall (DownstreamBackpressure pattern)

-- | Create a test with both upstream stalls and downstream backpressure
makeCombinedTest :: ByteString -> [Bool] -> [Bool] -> SHA3256Test
makeCombinedTest input stallPattern backpressurePattern =
  SHA3256Test
    input
    (UpstreamStall stallPattern)
    (DownstreamBackpressure backpressurePattern)
