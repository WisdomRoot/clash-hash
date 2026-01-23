{-# LANGUAGE OverloadedStrings #-}

module Test.PRF.Common
  ( PRFConfig (..),
    PRFTopEntity,
    prfSpec,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), property, vectorOf)
import Component.PRF.Common (Eta (..))
import Test.TestHarness.PRF (prfReference)
import Test.TestHarness.StreamCommon (bitListToBSHW, wordToBits)
import Prelude qualified as P

type PRFTopEntity =
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 264) ->
  Signal System Bool ->
  Signal System (AXI4Stream 64, Bool)

data PRFConfig = PRFConfig
  { pcName :: String,
    pcEta :: Eta,
    pcSeed :: ByteString,
    pcByte :: Word8,
    pcTopEntity :: PRFTopEntity
  }

data PRFInput = PRFInput
  { piSeed :: ByteString,
    piByte :: Word8
  }
  deriving (Show)

instance Arbitrary PRFInput where
  arbitrary = do
    seed <- BS.pack <$> vectorOf 32 arbitrary
    b <- arbitrary
    pure (PRFInput seed b)

prfSpec :: PRFConfig -> Spec
prfSpec cfg = describe (pcName cfg) $ do
  it ("matches kyber-py reference (eta=" <> show (etaValue (pcEta cfg)) <> ")") $ do
    let expected = prfReference (etaValue (pcEta cfg)) (pcSeed cfg) (pcByte cfg)
        actual = runHardware cfg (pcSeed cfg) (pcByte cfg)
    actual `shouldBe` expected

  it "QuickCheck matches kyber-py reference" $
    property (prop_prf cfg)

prop_prf :: PRFConfig -> PRFInput -> Bool
prop_prf cfg (PRFInput s b) =
  let expected = prfReference (etaValue (pcEta cfg)) s b
      actual = runHardware cfg s b
   in actual == expected

runHardware :: PRFConfig -> ByteString -> Word8 -> ByteString
runHardware cfg s b =
  let msgBV = bsToBV264 (s <> BS.singleton b)
      msgSig = pure msgBV
      treadySig = pure True
      output = pcTopEntity cfg clockGen resetGen enableGen msgSig treadySig
      etaWords = 8 P.* P.fromIntegral (etaValue (pcEta cfg))
      outputBytes = 64 P.* P.fromIntegral (etaValue (pcEta cfg))
      sampleCount = 24 P.+ 24 P.+ etaWords P.+ 200
      samples = sampleN @System sampleCount output
      validWords = [tdata stream | (stream, _) <- samples, tvalid stream]
      bits = P.concatMap wordToBits (P.take etaWords validWords)
   in BS.take outputBytes (bitListToBSHW bits)

etaValue :: Eta -> Word8
etaValue Eta2 = 2
etaValue Eta3 = 3

bsToBV264 :: ByteString -> BitVector 264
bsToBV264 bs =
  let padded = BS.take 33 (bs P.<> BS.replicate 33 0)
      bytes = BS.unpack padded
      step acc w =
        (acc `shiftL` 8) .|. resize (reverseBits8 (pack (fromIntegral w :: BitVector 8)))
   in P.foldl step (0 :: BitVector 264) bytes

reverseBits8 :: BitVector 8 -> BitVector 8
reverseBits8 bv = pack (reverse (unpack bv :: Vec 8 Bit))
