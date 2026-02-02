{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SHA3512Normal
  ( SHA3512NormalTest,
    sha3512NormalGen,
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
import Hash.NonPipelined.SHA3512Normal qualified as SHA3512Normal
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

type SHA3512NormalTest = ShakeTest

sha3512NormalParams :: ShakeParams
sha3512NormalParams =
  ShakeParams
    { spBeatsPerBlock = 9,
      spReference = const Crypton.sha3_512,
      spTopEntity = SHA3512Normal.topEntity
    }

sha3512NormalGenConfig :: ShakeGenConfig
sha3512NormalGenConfig =
  defaultShakeGenConfig
    { sgBeatOptions =
        [ (2, 1),
          (1, 2),
          (1, 8),
          (2, 9),
          (1, 10),
          (2, 17),
          (1, 18),
          (1, 25)
        ],
      sgOutputOptions = [(1, 64)]
    }

sha3512NormalGen :: Gen SHA3512NormalTest
sha3512NormalGen = genShakeTest sha3512NormalGenConfig

runTest :: SHA3512NormalTest -> Expectation
runTest test = do
  let expected = spReference sha3512NormalParams (Common.testOutputBytes test) (Common.testMessage test)
      actual = runHardware test
  actual `shouldBe` expected

runHardware :: SHA3512NormalTest -> ByteString
runHardware test =
  let inputBytes = BS.length (Common.testMessage test)
      beats = (inputBytes P.+ 7) `P.div` 8
   in case fromJust (someNatVal (P.fromIntegral beats)) of
        SomeNat (_ :: Proxy beats') ->
          runHardwareKnown @beats' sha3512NormalParams test beats (spBeatsPerBlock sha3512NormalParams)

testLabel :: SHA3512NormalTest -> String
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
