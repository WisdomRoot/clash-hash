module Test.TestHarness.SHA3256Normal
  ( SHA3256NormalTest,
    sha3256NormalGen,
    runTest,
    runHardware,
    testLabel,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Clash.Sized.Vector qualified as V
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Proxy (Proxy (..))
import Hash.NonPipelined.SHA3256Normal qualified as SHA3256Normal
import Reference.Crypton qualified as Crypton
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck (Gen, arbitrary, frequency, vector)
import Test.TestHarness.SHAKECommon (ShakeTest (..))
import Test.TestHarness.SHAKECommon qualified as Common
import Test.TestHarness.StreamCommon
  ( UpstreamStall (..),
    bitListToBSHW,
    bsToBitListHW,
    makeBackpressureSignal,
  )
import Prelude qualified as P

type SHA3256NormalTest = ShakeTest

type SHA3256NormalTopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream 1088, Bool) ->
  Signal System (AXI4Stream 544, Bool)

data SHA3256NormalParams = SHA3256NormalParams
  { spBeatsPerBlock :: Int,
    spReference :: Int -> ByteString -> ByteString,
    spTopEntity :: SHA3256NormalTopEntity
  }

sha3256NormalParams :: SHA3256NormalParams
sha3256NormalParams =
  SHA3256NormalParams
    { spBeatsPerBlock = 1,
      spReference = \_ msg -> Crypton.sha3 msg,
      spTopEntity = SHA3256Normal.topEntity
    }

sha3256NormalGen :: Gen SHA3256NormalTest
sha3256NormalGen = do
  blockCount <- frequency [(3, pure 1), (2, pure 2), (1, pure 3)]
  messageBytes <- BS.pack <$> vector (blockCount P.* 136)
  upstreamStall <- arbitrary
  downstreamBackpressure <- arbitrary
  pure
    ShakeTest
      { testMessage = messageBytes,
        testOutputBytes = 32,
        testUpstreamStall = upstreamStall,
        testDownstreamBackpressure = downstreamBackpressure
      }

runTest :: SHA3256NormalTest -> Expectation
runTest test = do
  let expected = spReference sha3256NormalParams (testOutputBytes test) (testMessage test)
      actual = runHardware test
  actual `shouldBe` expected

runHardware :: SHA3256NormalTest -> ByteString
runHardware = runSHA3256NormalHardware sha3256NormalParams

runSHA3256NormalHardware :: SHA3256NormalParams -> SHA3256NormalTest -> ByteString
runSHA3256NormalHardware params test =
  let inputBytes = BS.length (testMessage test)
      (beats, remBytes) = inputBytes `P.divMod` 136
      beatsPerBlock = spBeatsPerBlock params
   in if remBytes /= 0
        then P.error "SHA3256Normal: input length must be a multiple of 136 bytes"
        else case someNatVal (P.fromIntegral beats) of
          Just (SomeNat (_ :: Proxy beats')) ->
            runHardwareKnown @beats' params test beats beatsPerBlock
          Nothing -> P.error "SHA3256Normal: invalid beat count"

runHardwareKnown ::
  forall beats.
  (KnownNat beats) =>
  SHA3256NormalParams ->
  SHA3256NormalTest ->
  Int ->
  Int ->
  ByteString
runHardwareKnown params test beats beatsPerBlock =
  let inputBits = bsToBitListHW (testMessage test)
      paddedBits = P.take (beats P.* 1088) (inputBits P.++ P.repeat 0)
      messageWords = bitListToWords1088 @beats beats paddedBits
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ feedInput1088 @beats beatsPerBlock (testUpstreamStall test) messageWords
      treadySignal = makeBackpressureSignal (testDownstreamBackpressure test)
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          treadySignal
          inputStream
      outputBits = testOutputBytes test P.* 8
      outputBeats = (outputBits P.+ 543) `P.div` 544
      outputBeatsPerBlock = 2
      squeezesNeeded = (outputBeats P.+ outputBeatsPerBlock - 1) `P.div` outputBeatsPerBlock
      sampleCount =
        beats P.* 2
          P.+ 24
          P.+ squeezesNeeded P.* (outputBeatsPerBlock P.+ 24)
          P.+ 200
      samples = sampleN @System sampleCount output
      validOutputs = [tdata stream | (stream, _) <- samples, tvalid stream]
      outputWordBits = P.concatMap wordToBits544 (P.take outputBeats validOutputs)
      resultBits = P.take outputBits outputWordBits
   in bitListToBSHW resultBits

feedInput1088 ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  Int ->
  UpstreamStall ->
  Vec beats (BitVector 1088) ->
  Signal dom (AXI4Stream 1088, Bool)
feedInput1088 beatsPerBlock control messageWords =
  mealy step (toList messageWords, 0 :: Int, 0 :: Int, stallPattern, P.null messageWords) (pure ())
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

bitListToWords1088 :: forall beats. (KnownNat beats) => Int -> [Bit] -> Vec beats (BitVector 1088)
bitListToWords1088 n bits =
  let chunks = chunksOf 1088 bits
      wordsList = P.map bitsToWord (P.take n chunks)
   in V.unsafeFromList wordsList
  where
    chunksOf _ [] = []
    chunksOf m xs = P.take m xs : chunksOf m (P.drop m xs)
    bitsToWord bs =
      let paddedBits = P.take 1088 (bs P.++ P.repeat 0)
          -- word = P.foldl accumBit 0 (P.zip [1087, 1086 .. 0] paddedBits)
          word = P.foldl accumBit 0 (P.zip [0 .. 1087] paddedBits)
       in word
    accumBit acc (i, b) = if b == 1 then Bits.setBit acc i else acc

wordToBits544 :: BitVector 544 -> [Bit]
wordToBits544 w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 543]]

testLabel :: SHA3256NormalTest -> String
testLabel = Common.testLabel
