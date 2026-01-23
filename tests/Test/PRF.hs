{-# LANGUAGE OverloadedStrings #-}

module Test.PRF (spec) where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Component.PRF qualified as PRF
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.TestHarness.PRF (prfReference)
import Test.TestHarness.StreamCommon (bitListToBSHW, wordToBits)
import Prelude qualified as P

spec :: Spec
spec = describe "PRF" $ do
  it "eta=2 matches kyber-py reference" $ do
    let eta = 2 :: Word8
        s = BS.pack [0 .. 31]
        b = 0x01
        expected = prfReference eta s b
        actual = runHardware eta s b
    actual `shouldBe` expected

  it "eta=3 matches kyber-py reference" $ do
    let eta = 3 :: Word8
        s = BS.pack [31, 30 .. 0]
        b = 0xA5
        expected = prfReference eta s b
        actual = runHardware eta s b
    actual `shouldBe` expected

runHardware :: Word8 -> ByteString -> Word8 -> ByteString
runHardware eta s b =
  let msgBV = bsToBV264 (s <> BS.singleton b)
      etaSig = pure (fromIntegral eta :: Unsigned 2)
      msgSig = pure msgBV
      treadySig = pure True
      output = PRF.topEntity clockGen resetGen enableGen etaSig msgSig treadySig
      wordsNeeded = 8 P.* P.fromIntegral eta
      outputBytes = 64 P.* P.fromIntegral eta
      sampleCount = 24 P.+ 24 P.+ wordsNeeded P.+ 200
      samples = sampleN @System sampleCount output
      validWords = [tdata stream | (stream, _) <- samples, tvalid stream]
      bits = P.concatMap wordToBits (P.take wordsNeeded validWords)
   in BS.take outputBytes (bitListToBSHW bits)

bsToBV264 :: ByteString -> BitVector 264
bsToBV264 bs =
  let padded = BS.take 33 (bs P.<> BS.replicate 33 0)
      bytes = BS.unpack padded
      step acc w =
        (acc `shiftL` 8) .|. resize (reverseBits8 (pack (fromIntegral w :: BitVector 8)))
   in P.foldl step (0 :: BitVector 264) bytes

reverseBits8 :: BitVector 8 -> BitVector 8
reverseBits8 bv = pack (reverse (unpack bv :: Vec 8 Bit))
