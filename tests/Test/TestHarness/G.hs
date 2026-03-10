{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.G
  ( GTest,
    gGen,
    runTest,
    runHardware,
    testLabel,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.G qualified as G
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Reference.Crypton qualified as Crypton
import System.FilePath ((</>))
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, vectorOf)
import Test.TestHarness.ExternalReference (callPythonReference)
import Test.TestHarness.SHAKECommon (ShakeTest (..), UpstreamStall (..))
import Test.TestHarness.SHAKECommon qualified as Common
import Test.TestHarness.StreamCommon (bitListToBSHW, bsToBitListHW, makeBackpressureSignal)
import Prelude qualified as P

type GTest = ShakeTest

gGen :: Gen GTest
gGen = do
  bytes <- BS.pack <$> vectorOf 33 (arbitrary :: Gen Word8)
  upstream <- arbitrary
  downstream <- arbitrary
  pure $
    ShakeTest
      { testMessage = bytes,
        testOutputBytes = 32,
        testUpstreamStall = upstream,
        testDownstreamBackpressure = downstream
      }

runTest :: GTest -> Expectation
runTest test = do
  let outBytes = Common.testOutputBytes test
      msg = Common.testMessage test
      expectedCrypton = BS.take outBytes (Crypton.sha3_512 msg)
      expectedPython = BS.take outBytes (gReference msg)
      actual = runHardware test
  actual `shouldBe` expectedPython
  actual `shouldBe` expectedCrypton

runHardware :: GTest -> ByteString
runHardware test =
  let msg = Common.testMessage test
      msgBits = bsToBitListHW msg
      msgBV = bitsToWord272 msgBits
      inputStream =
        withClockResetEnable clockGen resetGen enableGen $
          feedInput272 (Common.testUpstreamStall test) msgBV
      treadySignal = makeBackpressureSignal (Common.testDownstreamBackpressure test)
      (msgSignal, _flushSignal) = unbundle inputStream
      output =
        G.i272o256
          clockGen
          resetGen
          enableGen
          (bundle (msgSignal, treadySignal))
      outputBits = Common.testOutputBytes test P.* 8
      outputBeats = (outputBits P.+ 255) `P.div` 256
      sampleCount =
        2
          P.+ 24
          P.+ outputBeats P.* (2 P.+ 24)
          P.+ 200
      samples = sampleN @System sampleCount output
      validOutputs = [tdata stream | (stream, _) <- samples, tvalid stream]
      outputWordBits = P.concatMap wordToBitsNormal256 (P.take outputBeats validOutputs)
      resultBits = P.take outputBits outputWordBits
   in bitListToBSHW resultBits

testLabel :: GTest -> P.String
testLabel = Common.testLabel

gReference :: ByteString -> ByteString
gReference input =
  let output = callPythonReference ("reference" </> "kyber" </> "g.py") input
   in BS.take 32 output

feedInput272 ::
  HiddenClockResetEnable dom =>
  UpstreamStall ->
  BitVector 272 ->
  Signal dom (AXI4Stream 272, Bool)
feedInput272 control msgWord = mealy step (P.True, stallPattern) (pure ())
  where
    stallPattern = case control of
      NoUpstreamStall -> []
      UpstreamStall xs -> xs

    step (pending, ctrl) _ =
      let (canSend, ctrl') = case ctrl of
            [] -> (P.True, [])
            b : bs -> (b, bs)
          idle =
            AXI4Stream
              { tdata = 0,
                tvalid = P.False,
                tlast = P.False
              }
       in if pending P.&& canSend
            then
              ( (P.False, ctrl'),
                ( AXI4Stream
                    { tdata = msgWord,
                      tvalid = P.True,
                      tlast = P.True
                    },
                  P.False
                )
              )
            else ((pending, ctrl'), (idle, P.False))

bitsToWord272 :: [Bit] -> BitVector 272
bitsToWord272 bits =
  let paddedBits = P.take 272 (bits P.++ P.repeat 0)
   in P.foldl accumBit 0 (P.zip [0 .. 271] paddedBits)
  where
    accumBit acc (i, b) = if b P.== 1 then Bits.setBit acc i else acc

wordToBitsNormal256 :: BitVector 256 -> [Bit]
wordToBitsNormal256 w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 255]]
