{-# LANGUAGE OverloadedStrings #-}

module Test.SamplePolyCBD512 (spec) where

import Component.PRF.Common (Eta (..))
import Component.SamplePolyCBD512 qualified as SamplePolyCBD512
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), property, vectorOf)
import Test.TestHarness.SamplePolyCBD
  ( DownstreamBackpressure (..),
    runSamplePolyCBDHardwareNormal,
    samplePolyCBDReference,
  )
import Prelude

data SamplePolyCBD512Input = SamplePolyCBD512Input
  { spcSeed :: BS.ByteString,
    spcByte :: Word8,
    spcBackpressure :: DownstreamBackpressure
  }
  deriving (Show)

instance Arbitrary SamplePolyCBD512Input where
  arbitrary = do
    seed <- BS.pack <$> vectorOf 32 arbitrary
    b <- arbitrary
    bp <- arbitrary
    pure (SamplePolyCBD512Input seed b bp)

spec :: Spec
spec = describe "SamplePolyCBD512" $ do
  let seed0 = BS.pack [0 .. 31]
      byte0 = 0x01

  it "matches kyber-py reference (seed=[0..31], b=0x01)" $ do
    let expected = samplePolyCBDReference Eta3 seed0 byte0
        actual = runSamplePolyCBDHardwareNormal SamplePolyCBD512.topEntity seed0 byte0 NoDownstreamBackpressure
    actual `shouldBe` expected

  it "matches kyber-py reference (all zeros)" $ do
    let seed = BS.replicate 32 0
        b = 0x00
        expected = samplePolyCBDReference Eta3 seed b
        actual = runSamplePolyCBDHardwareNormal SamplePolyCBD512.topEntity seed b NoDownstreamBackpressure
    actual `shouldBe` expected

  it "matches kyber-py reference (all ones)" $ do
    let seed = BS.replicate 32 0xFF
        b = 0xFF
        expected = samplePolyCBDReference Eta3 seed b
        actual = runSamplePolyCBDHardwareNormal SamplePolyCBD512.topEntity seed b NoDownstreamBackpressure
    actual `shouldBe` expected

  it "QuickCheck matches kyber-py reference" $
    property prop_samplePolyCBD512

prop_samplePolyCBD512 :: SamplePolyCBD512Input -> Bool
prop_samplePolyCBD512 (SamplePolyCBD512Input s b bp) =
  let expected = samplePolyCBDReference Eta3 s b
      actual = runSamplePolyCBDHardwareNormal SamplePolyCBD512.topEntity s b bp
   in actual == expected
