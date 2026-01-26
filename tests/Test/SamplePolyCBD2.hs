{-# LANGUAGE OverloadedStrings #-}

module Test.SamplePolyCBD2 (spec) where

import Component.PRF.Common (Eta (..))
import Component.SamplePolyCBD2 qualified as SamplePolyCBD2
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), property, vectorOf)
import Test.TestHarness.SamplePolyCBD
  ( DownstreamBackpressure (..),
    runSamplePolyCBDHardware,
    samplePolyCBDReference,
  )
import Prelude

data SamplePolyCBD2Input = SamplePolyCBD2Input
  { spcSeed :: BS.ByteString,
    spcByte :: Word8,
    spcBackpressure :: DownstreamBackpressure
  }
  deriving (Show)

instance Arbitrary SamplePolyCBD2Input where
  arbitrary = do
    seed <- BS.pack <$> vectorOf 32 arbitrary
    b <- arbitrary
    bp <- arbitrary
    pure (SamplePolyCBD2Input seed b bp)

spec :: Spec
spec = describe "SamplePolyCBD2" $ do
  let seed0 = BS.pack [0 .. 31]
      byte0 = 0x01

  it "matches kyber-py reference (seed=[0..31], b=0x01)" $ do
    let expected = samplePolyCBDReference Eta2 seed0 byte0
        actual = runSamplePolyCBDHardware SamplePolyCBD2.topEntity seed0 byte0 NoDownstreamBackpressure
    actual `shouldBe` expected

  it "matches kyber-py reference (all zeros)" $ do
    let seed = BS.replicate 32 0
        b = 0x00
        expected = samplePolyCBDReference Eta2 seed b
        actual = runSamplePolyCBDHardware SamplePolyCBD2.topEntity seed b NoDownstreamBackpressure
    actual `shouldBe` expected

  it "matches kyber-py reference (all ones)" $ do
    let seed = BS.replicate 32 0xFF
        b = 0xFF
        expected = samplePolyCBDReference Eta2 seed b
        actual = runSamplePolyCBDHardware SamplePolyCBD2.topEntity seed b NoDownstreamBackpressure
    actual `shouldBe` expected

  it "QuickCheck matches kyber-py reference" $
    property prop_samplePolyCBD2

prop_samplePolyCBD2 :: SamplePolyCBD2Input -> Bool
prop_samplePolyCBD2 (SamplePolyCBD2Input s b bp) =
  let expected = samplePolyCBDReference Eta2 s b
      actual = runSamplePolyCBDHardware SamplePolyCBD2.topEntity s b bp
   in actual == expected
