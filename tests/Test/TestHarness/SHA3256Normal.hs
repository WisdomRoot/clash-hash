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
  ( feedInput,
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
  Signal System (AXI4Stream 64, Bool) ->
  Signal System (AXI4Stream 64, Bool)

data SHA3256NormalParams = SHA3256NormalParams
  { spBeatsPerBlock :: Int,
    spReference :: Int -> ByteString -> ByteString,
    spTopEntity :: SHA3256NormalTopEntity
  }

sha3256NormalParams :: SHA3256NormalParams
sha3256NormalParams =
  SHA3256NormalParams
    { spBeatsPerBlock = 17,
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
      beatsPerBlock = spBeatsPerBlock params
      beats = (inputBytes P.+ 7) `P.div` 8
   in case someNatVal (P.fromIntegral beats) of
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
      paddedBits = P.take (beats P.* 64) (inputBits P.++ P.repeat 0)
      messageWords = bitListToWords64 @beats beats paddedBits
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ feedInput @beats beatsPerBlock (testUpstreamStall test) messageWords
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
      outputBeats = (outputBits P.+ 63) `P.div` 64
      squeezesNeeded = (outputBeats P.+ beatsPerBlock - 1) `P.div` beatsPerBlock
      sampleCount =
        beats P.* 2
          P.+ 24
          P.+ squeezesNeeded P.* (beatsPerBlock P.+ 24)
          P.+ 200
      samples = sampleN @System sampleCount output
      validOutputs = [tdata stream | (stream, _) <- samples, tvalid stream]
      outputWordBits = P.concatMap wordToBits64 (P.take outputBeats validOutputs)
      resultBits = P.take outputBits outputWordBits
   in bitListToBSHW resultBits

bitListToWords64 :: forall beats. (KnownNat beats) => Int -> [Bit] -> Vec beats (BitVector 64)
bitListToWords64 n bits =
  let chunks = chunksOf 64 bits
      wordsList = P.map bitsToWord (P.take n chunks)
   in V.unsafeFromList wordsList
  where
    chunksOf _ [] = []
    chunksOf m xs = P.take m xs : chunksOf m (P.drop m xs)
    bitsToWord bs =
      let paddedBits = P.take 64 (bs P.++ P.repeat 0)
          word = P.foldl accumBit 0 (P.zip [0 .. 63] paddedBits)
       in word
    accumBit acc (i, b) = if b == 1 then Bits.setBit acc i else acc

wordToBits64 :: BitVector 64 -> [Bit]
wordToBits64 w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 63]]

testLabel :: SHA3256NormalTest -> String
testLabel = Common.testLabel
