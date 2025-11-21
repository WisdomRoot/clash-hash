{-# LANGUAGE TypeApplications #-}

module KeccakF1600Spec (spec) where

import Clash.Prelude
import DUT (topEntity, driveMessage, driveMessageDebug)
import SHA3internal (BitString)
import qualified SHA3
import Test.Hspec
import qualified Prelude as P
import Clash.XException (isX)
import qualified Data.List as L

spec :: Spec
spec = describe "KeccakF1600 SHA3-256 Hardware" $ do
  it "all zeros (0x0000000000000000)" $ do
    testInput 0x0000000000000000

  it "all ones (0xFFFFFFFFFFFFFFFF)" $ do
    testInput 0xFFFFFFFFFFFFFFFF

  it "alternating pattern (0xAAAAAAAAAAAAAAAA)" $ do
    testInput 0xAAAAAAAAAAAAAAAA

  it "counter pattern (0x0001020304050607)" $ do
    testInput 0x0001020304050607

  it "single bit set (0x8000000000000000)" $ do
    testInput 0x8000000000000000

  -- Keep a lightweight liveness probe to catch regressions to hanging behaviour.
  it "debug: sees first valid output beat within 200 cycles (all zeros input)" $ do
    let inputMsg = 0x0000000000000000
        (_digestSig, _doneSig, _sReadySig, mValid, mData, _mLast, _beatNum) =
          driveMessageDebug topEntity inputMsg
        mValidSamples = sampleN @System @Bool 200 mValid
        mDataSamples  = sampleN @System @(BitVector 64) 200 mData
        firstValid = L.find (\(v, _) -> v) (P.zip mValidSamples mDataSamples)
    case firstValid of
      Nothing -> expectationFailure "No valid output beat observed within 200 cycles"
      Just (_v, d) ->
        case isX d of
          Left msg -> expectationFailure ("Output beat undefined: " <> msg)
          Right _  -> pure ()

-- | Test a single 64-bit input message against the DUT
testInput :: BitVector 64 -> Expectation
testInput inputMsg = do
  let expectedDigest = v2bv (SHA3.sha3_256 (bv2v inputMsg :: BitString 64))
  let (actualDigest, done) = driveMessage topEntity inputMsg
  let digestSamples = sampleN @System 400 actualDigest :: [BitVector 256]
  let doneSamples = sampleN @System 400 done :: [Bool]

  -- Verify we collected all output beats
  P.last doneSamples `shouldBe` True
  -- Verify digest matches reference
  P.last digestSamples `shouldBe` expectedDigest
