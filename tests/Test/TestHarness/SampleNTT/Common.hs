{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SampleNTT.Common
  ( UpstreamStall (..),
    DownstreamBackpressure (..),
    ShakeTest (..),
    testLabel,
    makeBasicTest,
    makeVariableOutputTest,
    makeStallTest,
    makeBackpressureTest,
    makeCombinedTest,
    externalSampleNTTPacked,
    getSampleNTTOutput,
    unpackPython384Bytes,
    getTimingInfo,
    simulateTimingN,
    simulateTiming,
    simulateTiming2,
    makeUpstreamValidSignal,
    makeBackpressureSignalRepeat,
    backpressurePattern,
    bsToBV272,
    bsToBV272Normal,
    reverseBits12Word,
    splitCoeffsN,
    sampleCountMargin,
  )
where

import Clash.Prelude
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word16, Word8)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
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

sampleCountMargin :: Int
sampleCountMargin = 50

sampleNTTScriptPath :: FilePath
sampleNTTScriptPath = "reference" </> "kyber" </> "sample_ntt.py"

data SampleNTTJson = SampleNTTJson
  { sampleNTTPacked :: [Word8],
    sampleNTTValidity :: [Bool]
  }

instance FromJSON SampleNTTJson where
  parseJSON = withObject "SampleNTTJson" $ \o ->
    SampleNTTJson <$> o .: "packed" <*> o .: "validity"

getSampleNTTOutput :: ByteString -> (ByteString, [Bool])
getSampleNTTOutput input = unsafePerformIO $ do
  (Just hIn, Just hOut, _, ph) <-
    createProcess
      (proc "python3" [sampleNTTScriptPath])
        { std_in = CreatePipe,
          std_out = CreatePipe
        }
  BS.hPut hIn input
  hClose hIn
  output <- LBS.hGetContents hOut
  _ <- waitForProcess ph
  case eitherDecode output of
    Left err -> P.error $ "Failed to parse SampleNTT JSON: " P.++ err
    Right parsed ->
      P.return
        ( BS.pack (sampleNTTPacked parsed),
          sampleNTTValidity parsed
        )
{-# NOINLINE getSampleNTTOutput #-}

externalSampleNTTPacked :: ByteString -> ByteString
externalSampleNTTPacked = P.fst . getSampleNTTOutput

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

-- | Get validity pattern from Python reference script
-- Returns which coefficients pass rejection sampling (< 3329)
getTimingInfo :: ByteString -> [Bool]
getTimingInfo = P.snd . getSampleNTTOutput
{-# NOINLINE getTimingInfo #-}

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

bsToBV272Normal :: ByteString -> BitVector 272
bsToBV272Normal bs =
  let padded = BS.take 34 (bs P.<> BS.replicate 34 0)
      bits = P.concatMap word8ToBits (BS.unpack padded)
      paddedBits = P.take 272 (bits P.++ P.repeat 0)
   in P.foldl accumBit 0 (P.zip [0 .. 271] paddedBits)
  where
    word8ToBits :: Word8 -> [Bit]
    word8ToBits w = [if testBit w i then 1 else 0 | i <- [0 .. 7]]
    accumBit acc (i, b) = if b == 1 then setBit acc i else acc

reverseBits12Word :: Word16 -> Word16
reverseBits12Word w =
  let bv = pack (fromIntegral w :: Unsigned 12)
      rev = pack (reverse (unpack bv :: Vec 12 Bit))
   in P.fromIntegral (unpack rev :: Unsigned 12)

splitCoeffsN :: forall n. (KnownNat n) => BitVector (n * 12) -> [Word16]
splitCoeffsN bv =
  let chunks = unconcatI @n @12 (unpack bv :: Vec (n * 12) Bit)
      coeffs =
        P.map
          (P.fromIntegral . (unpack :: BitVector 12 -> Unsigned 12) . pack)
          (toList chunks)
   in P.reverse coeffs

-- | Simulate exact timing based on validity pattern and stall/backpressure
-- Returns the exact number of cycles needed to collect 256 valid coefficients
simulateTimingN ::
  Int ->
  [Bool] ->
  UpstreamStall ->
  DownstreamBackpressure ->
  Int
simulateTimingN coeffsPerBeat validityPattern upstreamStall backpressure =
  let upstreamStallCycles = case upstreamStall of
        NoUpstreamStall -> 0
        UpstreamStall pattern -> P.length (P.takeWhile P.id pattern)
      bpPattern = P.cycle (backpressurePattern backpressure)
      beatsPerBlock =
        case 112 `P.mod` coeffsPerBeat of
          0 -> 112 `P.div` coeffsPerBeat
          _ -> P.error "simulateTimingN: coeffsPerBeat must divide 112"
      consumeVals :: Int -> [Bool] -> (Int, Bool)
      consumeVals bufferLen vals = go bufferLen False vals
        where
          go bl emitted [] = (bl, emitted)
          go bl emitted (v : vs) =
            let bl' = if v then bl P.+ 1 else bl
             in if emitted
                  then go bl' True vs
                  else
                    if bl' P.>= coeffsPerBeat
                      then go (bl' P.- coeffsPerBeat) True vs
                      else go bl' False vs
      simulateSqueeze ::
        Int ->
        Int ->
        [Bool] ->
        [Bool] ->
        (Int, Int, Int, [Bool], [Bool])
      simulateSqueeze = go (0 :: Int) (0 :: Int)
        where
          go cycles squeezeIdx validCnt bufferLen val bpPat
            | validCnt P.>= 256 = (cycles, validCnt, bufferLen, bpPat, val)
            | squeezeIdx P.>= beatsPerBlock = (cycles, validCnt, bufferLen, bpPat, val)
            | P.otherwise =
                let (tready, bpPat') = case bpPat of
                      [] -> (True, [])
                      (b : bs) -> (b, bs)
                 in if P.not tready
                      then go (cycles P.+ 1) squeezeIdx validCnt bufferLen val bpPat'
                      else
                        let (vals, val') = P.splitAt coeffsPerBeat val
                            (bufferLen', emitted) = consumeVals bufferLen vals
                            validCnt' = if emitted then validCnt P.+ coeffsPerBeat else validCnt
                         in if P.length vals P.< coeffsPerBeat P.&& validCnt' P.< 256
                              then P.error "simulateTimingN: validity pattern exhausted"
                              else go (cycles P.+ 1) (squeezeIdx P.+ 1) validCnt' bufferLen' val' bpPat'
      simulateBlocks ::
        Int ->
        Int ->
        Int ->
        [Bool] ->
        [Bool] ->
        Int
      simulateBlocks totalCycles validCount bufferLen validity bp
        | validCount P.>= 256 = totalCycles
        | P.otherwise =
            let afterPermute = totalCycles P.+ 24
                (squeezeCycles, newValidCount, newBufferLen, newBp, newValidity) =
                  simulateSqueeze validCount bufferLen validity bp
             in simulateBlocks (afterPermute P.+ squeezeCycles) newValidCount newBufferLen newValidity newBp
      initialCycles = 1 P.+ upstreamStallCycles P.+ 1
      bpAfterInit = P.drop initialCycles bpPattern
   in simulateBlocks initialCycles 0 0 validityPattern bpAfterInit

simulateTiming ::
  [Bool] ->
  UpstreamStall ->
  DownstreamBackpressure ->
  Int
simulateTiming validityPattern upstreamStall backpressure =
  simulateTimingN 1 validityPattern upstreamStall backpressure

simulateTiming2 ::
  [Bool] ->
  UpstreamStall ->
  DownstreamBackpressure ->
  Int
simulateTiming2 validityPattern upstreamStall backpressure =
  simulateTimingN 2 validityPattern upstreamStall backpressure
