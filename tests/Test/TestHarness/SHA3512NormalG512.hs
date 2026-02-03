{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.SHA3512NormalG512
  ( SHA3512NormalG512Test,
    sha3512NormalG512Gen,
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
import Component.G512 qualified as G512
import Reference.Crypton qualified as Crypton
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck (Gen)
import Test.TestHarness.SHAKECommon
  ( ShakeGenConfig (..),
    ShakeTest (..),
    defaultShakeGenConfig,
    genShakeTest
  )
import Test.TestHarness.SHAKECommon qualified as Common
import Test.TestHarness.StreamCommon
  ( bitListToBSHW,
    bsToBitListHW,
    feedInput256,
    makeBackpressureSignal
  )
import Prelude qualified as P

type SHA3512NormalG512Test = ShakeTest

type ShakeTopEntity256 =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream 256, Bool) ->
  Signal System (AXI4Stream 256, Bool)

data ShakeParams256 = ShakeParams256
  { spBeatsPerBlock :: Int,
    spReference :: Int -> ByteString -> ByteString,
    spTopEntity :: ShakeTopEntity256
  }

sha3512NormalG512Params :: ShakeParams256
sha3512NormalG512Params =
  ShakeParams256
    { spBeatsPerBlock = 3,
      spReference = \outBytes msg -> BS.take outBytes (Crypton.sha3_512 (msg <> BS.pack [2])),
      spTopEntity = G512.topEntity
    }

sha3512NormalG512GenConfig :: ShakeGenConfig
sha3512NormalG512GenConfig =
  defaultShakeGenConfig
    { sgBeatOptions =
        [ (1, 4)
        ],
      sgBeatRanges = [],
      sgOutputOptions = [(1, 32)]
    }

sha3512NormalG512Gen :: Gen SHA3512NormalG512Test
sha3512NormalG512Gen = genShakeTest sha3512NormalG512GenConfig

runTest :: SHA3512NormalG512Test -> Expectation
runTest test = do
  let expected = spReference sha3512NormalG512Params (Common.testOutputBytes test) (Common.testMessage test)
      actual = runHardware test
  actual `shouldBe` expected

runHardware :: SHA3512NormalG512Test -> ByteString
runHardware test =
  let inputBytes = BS.length (Common.testMessage test)
      beats = (inputBytes P.+ 31) `P.div` 32
   in case fromJust (someNatVal (P.fromIntegral beats)) of
        SomeNat (_ :: Proxy beats') ->
          runHardwareKnown @beats' sha3512NormalG512Params test beats (spBeatsPerBlock sha3512NormalG512Params)

testLabel :: SHA3512NormalG512Test -> String
testLabel = Common.testLabel

runHardwareKnown ::
  forall beats.
  (KnownNat beats) =>
  ShakeParams256 ->
  ShakeTest ->
  Int ->
  Int ->
  ByteString
runHardwareKnown params test beats beatsPerBlock =
  let inputBS = Common.testMessage test
      inputBits = bsToBitListHW inputBS
      paddedBits = P.take (beats P.* 256) (inputBits P.++ P.repeat 0)
      messageWords = bitListToWordsNormal256 @beats beats paddedBits
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ feedInput256 @beats beatsPerBlock (Common.testUpstreamStall test) messageWords
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
      outputBeats = (outputBits P.+ 255) `P.div` 256
      squeezesNeeded = (outputBeats P.+ beatsPerBlock - 1) `P.div` beatsPerBlock
      sampleCount =
        beats P.* 2
          P.+ 24
          P.+ squeezesNeeded P.* (beatsPerBlock P.+ 24)
          P.+ 200
      samples = sampleN @System sampleCount output
      validOutputs = [tdata stream | (stream, _) <- samples, tvalid stream]
      outputWordBits = P.concatMap wordToBitsNormal256 (P.take outputBeats validOutputs)
      resultBits = P.take outputBits outputWordBits
   in bitListToBSHW resultBits

bitListToWordsNormal256 :: forall beats. (KnownNat beats) => Int -> [Bit] -> Vec beats (BitVector 256)
bitListToWordsNormal256 n bits =
  let chunks = chunksOf 256 bits
      wordsList = P.map bitsToWord (P.take n chunks)
   in V.unsafeFromList wordsList
  where
    chunksOf _ [] = []
    chunksOf m xs = P.take m xs : chunksOf m (P.drop m xs)
    bitsToWord bs =
      let paddedBits = P.take 256 (bs P.++ P.repeat 0)
          word = P.foldl accumBit 0 (P.zip [0 .. 255] paddedBits)
       in word
    accumBit acc (i, b) = if b == 1 then Bits.setBit acc i else acc

wordToBitsNormal256 :: BitVector 256 -> [Bit]
wordToBitsNormal256 w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 255]]
