{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.G.Common
  ( gReference,
    gReferenceK,
    GTest,
    gGen,
    runTest,
    runHardware,
    testLabel,
    gParamsFor
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
import Parameter (MLKEM (..))
import Component.G2 qualified as G2
import Component.G3 qualified as G3
import Component.G4 qualified as G4
import Reference.Crypton qualified as Crypton
import System.FilePath ((</>))
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck (Gen)
import Test.TestHarness.ExternalReference (callPythonReference)
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

type GTest = ShakeTest

type O256 =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 256, Bool) ->
  Signal System (AXI4Stream 256, Bool)

type O512 =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 256, Bool) ->
  Signal System (AXI4Stream 512, Bool)

data Backend
  = Backend256 O256
  | Backend512 O512

data Params = Params
  { gpBeatsPerBlock :: Int,
    gpKByte :: Word8,
    gpReference :: Int -> ByteString -> ByteString,
    gpBackend :: Backend
  }

-- | Reference implementation of G (SHA3-512 split into two 32-byte outputs)
--
-- G: B* → B^32 × B^32
--
-- Input: Variable-length bytes
-- Output: (rho, sigma) where each is 32 bytes

gReference :: ByteString -> (ByteString, ByteString)
gReference = gReferenceK 2

-- | Reference implementation of G with explicit k (2/3/4).
gReferenceK :: Word8 -> ByteString -> (ByteString, ByteString)
gReferenceK k input =
  let output = callPythonReference ("reference" </> "kyber" </> "g.py") (input P.<> BS.pack [k])
   in (BS.take 32 output, BS.drop 32 output)

gParamsFor :: MLKEM -> Params
gParamsFor mlkem =
  let kByte = mlkemToKByte mlkem
   in Params
        { gpBeatsPerBlock = 3,
          gpKByte = kByte,
          gpReference = \outBytes msg -> BS.take outBytes (Crypton.sha3_512 (msg <> BS.pack [kByte])),
          gpBackend = mlkemBackend mlkem
        }

mlkemToKByte :: MLKEM -> Word8
mlkemToKByte MLKEM512 = 2
mlkemToKByte MLKEM768 = 3
mlkemToKByte MLKEM1024 = 4

mlkemBackend :: MLKEM -> Backend
mlkemBackend MLKEM512 = Backend512 G2.i256o512
mlkemBackend MLKEM768 = Backend512 G3.i256o512
mlkemBackend MLKEM1024 = Backend512 G4.i256o512

gGenConfig :: ShakeGenConfig
gGenConfig =
  defaultShakeGenConfig
    { sgBeatOptions =
        [ (1, 4)
        ],
      sgBeatRanges = [],
      sgOutputOptions = [(1, 32)]
    }

gGen :: Gen GTest
gGen = genShakeTest gGenConfig

runTest :: Params -> GTest -> Expectation
runTest params test = do
  let outBytes = Common.testOutputBytes test
      msg = Common.testMessage test
      expectedCrypton = gpReference params outBytes msg
      (rho, _sigma) = gReferenceK (gpKByte params) msg
      expectedPython = BS.take outBytes rho
      actual = runHardware params test
  actual `shouldBe` expectedPython
  actual `shouldBe` expectedCrypton

runHardware :: Params -> GTest -> ByteString
runHardware params test =
  let inputBytes = BS.length (Common.testMessage test)
      beats = (inputBytes P.+ 31) `P.div` 32
   in case fromJust (someNatVal (P.fromIntegral beats)) of
        SomeNat (_ :: Proxy beats') ->
          runHardwareKnown @beats' params test beats (gpBeatsPerBlock params)

testLabel :: GTest -> P.String
testLabel = Common.testLabel

runHardwareKnown ::
  forall beats.
  (KnownNat beats) =>
  Params ->
  GTest ->
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
      (msgSignal, _flushSignal) = unbundle inputStream
      outputBits = Common.testOutputBytes test P.* 8
   in case gpBackend params of
        Backend256 topEntity ->
          let output =
                topEntity
                  clockGen
                  resetGen
                  enableGen
                  (bundle (msgSignal, treadySignal))
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
        Backend512 topEntity ->
          let output =
                topEntity
                  clockGen
                  resetGen
                  enableGen
                  (bundle (msgSignal, treadySignal))
              outputBeats = (outputBits P.+ 511) `P.div` 512
              squeezesNeeded = (outputBeats P.+ beatsPerBlock - 1) `P.div` beatsPerBlock
              sampleCount =
                beats P.* 2
                  P.+ 24
                  P.+ squeezesNeeded P.* (beatsPerBlock P.+ 24)
                  P.+ 200
              samples = sampleN @System sampleCount output
              validOutputs = [tdata stream | (stream, _) <- samples, tvalid stream]
              outputWordBits = P.concatMap wordToBitsNormal512 (P.take outputBeats validOutputs)
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

wordToBitsNormal512 :: BitVector 512 -> [Bit]
wordToBitsNormal512 w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 511]]
