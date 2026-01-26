{-# LANGUAGE OverloadedStrings #-}

module Test.SamplePolyCBD3 (spec) where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.PRF.Common (Eta (..))
import Component.SamplePolyCBD3 qualified as SamplePolyCBD3
import Data.ByteString qualified as BS
import Data.Word (Word8, Word16)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), property, vectorOf)
import Test.TestHarness.SamplePolyCBD (samplePolyCBDReference)
import Prelude qualified as P

data SamplePolyCBD3Input = SamplePolyCBD3Input
  { spcSeed :: BS.ByteString,
    spcByte :: Word8
  }
  deriving (Show)

instance Arbitrary SamplePolyCBD3Input where
  arbitrary = do
    seed <- BS.pack P.<$> vectorOf 32 arbitrary
    b <- arbitrary
    P.pure (SamplePolyCBD3Input seed b)

spec :: Spec
spec = describe "SamplePolyCBD3" $ do
  let seed0 = BS.pack [0 .. 31]
      byte0 = 0x01

  it "matches kyber-py reference (seed=[0..31], b=0x01)" $ do
    let expected = samplePolyCBDReference Eta3 seed0 byte0
        actual = runSamplePolyCBD3Hardware SamplePolyCBD3.topEntity seed0 byte0
    actual `shouldBe` expected

  it "matches kyber-py reference (all zeros)" $ do
    let seed = BS.replicate 32 0
        b = 0x00
        expected = samplePolyCBDReference Eta3 seed b
        actual = runSamplePolyCBD3Hardware SamplePolyCBD3.topEntity seed b
    actual `shouldBe` expected

  it "matches kyber-py reference (all ones)" $ do
    let seed = BS.replicate 32 0xFF
        b = 0xFF
        expected = samplePolyCBDReference Eta3 seed b
        actual = runSamplePolyCBD3Hardware SamplePolyCBD3.topEntity seed b
    actual `shouldBe` expected

  it "QuickCheck matches kyber-py reference" $
    property prop_samplePolyCBD3

prop_samplePolyCBD3 :: SamplePolyCBD3Input -> Bool
prop_samplePolyCBD3 (SamplePolyCBD3Input s b) =
  let expected = samplePolyCBDReference Eta3 s b
      actual = runSamplePolyCBD3Hardware SamplePolyCBD3.topEntity s b
   in actual P.== expected

type SamplePolyCBD3TopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 264) ->
  Signal System Bool ->
  Signal System (AXI4Stream 12, Bool)

runSamplePolyCBD3Hardware ::
  SamplePolyCBD3TopEntity ->
  BS.ByteString ->
  Word8 ->
  [Word16]
runSamplePolyCBD3Hardware topEntity s b =
  let msgBV = bsToBV264 (s P.<> BS.singleton b)
      msgSig = pure msgBV
      treadySignal = pure True
      output =
        topEntity
          clockGen
          resetGen
          enableGen
          msgSig
          treadySignal
      -- For eta=3: absorb + permute (24) + squeeze/load + permute (24) + squeeze + margin
      -- Need more cycles due to second permutation and more words
      sampleCount = 24 P.+ 24 P.+ 24 P.+ 256 P.+ 300
      samples = sampleN @System sampleCount (bundle (output, treadySignal))
      validOutputs =
        [ tdata stream
          | ((stream, _), ready) <- samples,
            tvalid stream P.&& ready
        ]
      coeffs =
        P.map (P.fromIntegral . (unpack :: BitVector 12 -> Unsigned 12)) (P.take 256 validOutputs)
   in coeffs

bsToBV264 :: BS.ByteString -> BitVector 264
bsToBV264 bs =
  let padded = BS.take 33 (bs P.<> BS.replicate 33 0)
      bytes = BS.unpack padded
      step acc w =
        (acc `shiftL` 8) .|. resize (reverseBits8 (pack (fromIntegral w :: BitVector 8)))
   in P.foldl step (0 :: BitVector 264) bytes

reverseBits8 :: BitVector 8 -> BitVector 8
reverseBits8 bv = pack (reverse (unpack bv :: Vec 8 Bit))
