{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SHAKE128B
  ( ShakeTest,
    runTest,
    runHardware,
    testLabel
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Hash.NonPipelined.SHAKE128B qualified as SHAKE128B
import Reference.Crypton qualified as Crypton
import Test.Hspec (Expectation, shouldBe)
import Test.TestHarness.SHAKECommon (ShakeTest (..))
import Test.TestHarness.SHAKECommon qualified as Common
import Test.TestHarness.StreamCommon
  ( UpstreamStall (..),
    bitListToBSHW,
    bsToBitListHW,
    makeBackpressureSignal,
  )
import Prelude qualified as P
import Clash.Sized.Vector qualified as V

testLabel :: ShakeTest -> String
testLabel = Common.testLabel

runTest :: ShakeTest -> Expectation
runTest test = do
  let expected = Crypton.shake128 (Common.testOutputBytes test) (Common.testMessage test)
      actual = runHardware test
  actual `shouldBe` expected

runHardware :: ShakeTest -> ByteString
runHardware test =
  let inputBytes = BS.length (Common.testMessage test)
      beats = inputBytes -- For 8-bit stream, 1 beat per byte
   in case fromJust (someNatVal (P.fromIntegral beats)) of
        SomeNat (_ :: Proxy beats') ->
          runHardwareKnown @beats' test beats

runHardwareKnown ::
  forall beats.
  (KnownNat beats) =>
  ShakeTest ->
  Int ->
  ByteString
runHardwareKnown test beats =
  let beatsPerBlock = 168 -- 168 bytes = 1344 bits (SHAKE128 rate)
      inputBS = Common.testMessage test
      inputBits = bsToBitListHW inputBS
      -- Convert [Bit] to [Bool]
      inputBools = P.map (== 1) inputBits
      paddedBits = P.take (beats P.* 8) (inputBools P.++ P.repeat False) -- 8 bits per beat
      messageWords = bitListToWords8 @beats beats paddedBits
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ feedInput8 @beats beatsPerBlock (Common.testUpstreamStall test) messageWords
      treadySignal = makeBackpressureSignal (Common.testDownstreamBackpressure test)
      output =
        SHAKE128B.topEntity
          clockGen
          resetGen
          enableGen
          treadySignal
          inputStream
      outputBits = Common.testOutputBytes test P.* 8
      outputBeats = outputBits `P.div` 8 -- 8 bits per beat
      squeezesNeeded = (outputBeats P.+ beatsPerBlock - 1) `P.div` beatsPerBlock
      sampleCount =
        beats P.* 2
          P.+ 24
          P.+ squeezesNeeded P.* (beatsPerBlock P.+ 24)
          P.+ 200
      samples = sampleN @System sampleCount output
      validOutputs = [tdata stream | (stream, _) <- samples, tvalid stream]
      outputWordBits = P.concatMap wordToBits8 (P.take outputBeats validOutputs)
      resultBool = P.take outputBits outputWordBits
      resultBits = P.map (\b -> if b then 1 else 0) resultBool
      result = bitListToBSHW resultBits
   in result

--------------------------------------------------------------------------------
-- 8-bit Stream Helpers
--------------------------------------------------------------------------------

-- | Feed input as 8-bit AXI4-Stream
feedInput8 ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  Int ->
  UpstreamStall ->
  Vec beats (BitVector 8) ->
  Signal dom (AXI4Stream 8, Bool)
feedInput8 beatsPerBlock control messageWords =
  let isEmpty = V.length messageWords == 0
   in mealy step (toList messageWords, 0 :: Int, 0 :: Int, stallPattern, isEmpty) (pure ())
  where
    stallPattern = case control of
      NoUpstreamStall -> []
      UpstreamStall xs -> xs
    permuteLatency = 24 :: Int
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
                  []
                    | wasEmpty ->
                        (([], 0, 0, ctrl', False), (idleBeat, True))
                  [] -> (([], 0, 0, ctrl', False), (idleBeat, False))
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

-- | Convert bit list to 8-bit words
bitListToWords8 :: forall n. (KnownNat n) => Int -> [Bool] -> Vec n (BitVector 8)
bitListToWords8 beats bits =
  let chunks = splitIntoChunks 8 bits
      words8 = P.map bitsToWord (P.take beats chunks)
   in V.unsafeFromList words8
  where
    splitIntoChunks :: Int -> [Bool] -> [[Bool]]
    splitIntoChunks _ [] = []
    splitIntoChunks size xs =
      let (chunk, rest) = P.splitAt size xs
       in chunk : splitIntoChunks size rest

    bitsToWord :: [Bool] -> BitVector 8
    bitsToWord bs =
      let paddedBits = P.take 8 (bs P.++ P.repeat False)
          accumBit acc (i, b) = if b then setBit acc i else acc
       in P.foldl accumBit 0 (P.zip [7, 6 .. 0] paddedBits)

-- | Convert 8-bit word to bits (MSB first, matching 64-bit version)
wordToBits8 :: BitVector 8 -> [Bool]
wordToBits8 word = [testBit word i | i <- [7, 6 .. 0]]
