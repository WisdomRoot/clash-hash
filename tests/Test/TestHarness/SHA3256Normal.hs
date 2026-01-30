{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SHA3256Normal
  ( SHA3256NormalTest,
    sha3256NormalGen,
    runTest,
    runHardware,
    testLabel
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
import Hash.NonPipelined.SHA3256Normal qualified as SHA3256Normal
import Reference.Crypton qualified as Crypton
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck (Gen)
import Test.TestHarness.SHAKECommon
  ( ShakeGenConfig (..),
    ShakeParams (..),
    ShakeTest (..),
    defaultShakeGenConfig,
    genShakeTest
  )
import Test.TestHarness.SHAKECommon qualified as Common
import Test.TestHarness.StreamCommon
  ( bitListToBSHW,
    bsToBitListHW,
    feedInput,
    makeBackpressureSignal
  )
import Prelude qualified as P

type SHA3256NormalTest = ShakeTest

sha3256NormalParams :: ShakeParams
sha3256NormalParams =
  ShakeParams
    { spBeatsPerBlock = 17,
      spReference = const Crypton.sha3,
      spTopEntity = SHA3256Normal.topEntity
    }

sha3256NormalGenConfig :: ShakeGenConfig
sha3256NormalGenConfig =
  defaultShakeGenConfig
    { sgBeatOptions =
        [ (2, 1),
          (1, 2),
          (1, 16),
          (2, 17),
          (1, 18),
          (2, 25),
          (1, 34),
          (1, 51)
        ],
      sgOutputOptions = [(1, 32)]
    }

sha3256NormalGen :: Gen SHA3256NormalTest
sha3256NormalGen = genShakeTest sha3256NormalGenConfig

runTest :: SHA3256NormalTest -> Expectation
runTest test = do
  let expected = spReference sha3256NormalParams (Common.testOutputBytes test) (Common.testMessage test)
      actual = runHardware test
  actual `shouldBe` expected

runHardware :: SHA3256NormalTest -> ByteString
runHardware test =
  let inputBytes = BS.length (Common.testMessage test)
      beats = (inputBytes P.+ 7) `P.div` 8
   in case fromJust (someNatVal (P.fromIntegral beats)) of
        SomeNat (_ :: Proxy beats') ->
          runHardwareKnown @beats' sha3256NormalParams test beats (spBeatsPerBlock sha3256NormalParams)

testLabel :: SHA3256NormalTest -> String
testLabel = Common.testLabel

runHardwareKnown ::
  forall beats.
  (KnownNat beats) =>
  ShakeParams ->
  ShakeTest ->
  Int ->
  Int ->
  ByteString
runHardwareKnown params test beats beatsPerBlock =
  let inputBS = Common.testMessage test
      inputBits = bsToBitListHW inputBS
      paddedBits = P.take (beats P.* 64) (inputBits P.++ P.repeat 0)
      messageWords = bitListToWordsNormal @beats beats paddedBits
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ feedInput @beats beatsPerBlock (Common.testUpstreamStall test) messageWords
      treadySignal = makeBackpressureSignal (Common.testDownstreamBackpressure test)
      output =
        spTopEntity
          params
          clockGen
          resetGen
          enableGen
          treadySignal
          inputStream
      outputBits = Common.testOutputBytes test P.* 8
      outputBeats = (outputBits P.+ 63) `P.div` 64
      squeezesNeeded = (outputBeats P.+ beatsPerBlock - 1) `P.div` beatsPerBlock
      sampleCount =
        beats P.* 2
          P.+ 24
          P.+ squeezesNeeded P.* (beatsPerBlock P.+ 24)
          P.+ 200
      samples = sampleN @System sampleCount output
      validOutputs = [tdata stream | (stream, _) <- samples, tvalid stream]
      outputWordBits = P.concatMap wordToBitsNormal (P.take outputBeats validOutputs)
      resultBits = P.take outputBits outputWordBits
   in bitListToBSHW resultBits

bitListToWordsNormal :: forall beats. (KnownNat beats) => Int -> [Bit] -> Vec beats (BitVector 64)
bitListToWordsNormal n bits =
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

wordToBitsNormal :: BitVector 64 -> [Bit]
wordToBitsNormal w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 63]]
