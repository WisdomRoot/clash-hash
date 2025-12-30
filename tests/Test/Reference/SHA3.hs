{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Reference.SHA3 (spec) where

import Clash.Prelude
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA3_256)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Prelude (String, ($))
import Prelude qualified as P
import Reference.SHA3 qualified as SHA3
import Reference.SHA3internal qualified as SHA3internal
import Test.Hspec

spec :: Spec
spec = describe "Reference SHA3-256 Tests" $ do
  it "Empty input" $ do
    let input = SHA3internal.toBitString $(listToVecTH "")
        result = bitStringToByteString (SHA3.sha3_256 input)
        expected = sha3_256_crypton ""
    result `shouldBe` expected

  it "Input: abc" $ do
    let input = SHA3internal.toBitString $(listToVecTH "abc")
        result = bitStringToByteString (SHA3.sha3_256 input)
        expected = sha3_256_crypton "abc"
    result `shouldBe` expected

  it "Input: qwertyui" $ do
    let input = SHA3internal.toBitString $(listToVecTH "qwertyui")
        result = bitStringToByteString (SHA3.sha3_256 input)
        expected = sha3_256_crypton "qwertyui"
    result `shouldBe` expected

  it "Input: qwertyuiopasdfgh" $ do
    let input = SHA3internal.toBitString $(listToVecTH "qwertyuiopasdfgh")
        result = bitStringToByteString (SHA3.sha3_256 input)
        expected = sha3_256_crypton "qwertyuiopasdfgh"
    result `shouldBe` expected

  it "Input: test" $ do
    let input = SHA3internal.toBitString $(listToVecTH "test")
        result = bitStringToByteString (SHA3.sha3_256 input)
        expected = sha3_256_crypton "test"
    result `shouldBe` expected

  it "Input: The quick brown fox jumps over the lazy dog" $ do
    let input = SHA3internal.toBitString $(listToVecTH "The quick brown fox jumps over the lazy dog")
        result = bitStringToByteString (SHA3.sha3_256 input)
        expected = sha3_256_crypton "The quick brown fox jumps over the lazy dog"
    result `shouldBe` expected

-- | Compute SHA3-256 using crypton library
sha3_256_crypton :: ByteString -> ByteString
sha3_256_crypton input =
  convert (hash input :: Digest SHA3_256)

-- | Convert BitString to ByteString (LSB first for each byte)
bitStringToByteString :: forall n. (KnownNat n) => SHA3internal.BitString n -> ByteString
bitStringToByteString bits =
  let bitsList = toList bits
      bytes = bitsToBytes bitsList
   in BS.pack bytes
  where
    bitsToBytes :: [Bit] -> [Word8]
    bitsToBytes [] = []
    bitsToBytes bs =
      let (chunk, rest) = P.splitAt 8 bs
          paddedChunk = P.take 8 (chunk P.++ P.repeat 0)
          byte = P.foldl accumBit 0 (P.zip [0 ..] paddedChunk)
       in byte : bitsToBytes rest

    accumBit :: Word8 -> (Int, Bit) -> Word8
    accumBit acc (i, b) = if b P.== 1 then setBit acc i else acc
