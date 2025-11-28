{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Sponge (spec) where

import Clash.Prelude
import qualified Prelude as P
import qualified KeccakF1600.Permutation as Perm
import qualified Sponge.Pure
import Test.Hspec
import qualified SHA3
import SHA3internal (toBitString)

spec :: Spec
spec = fdescribe "Sponge (Pure)" $ do
  let sha3Suffix = 0b01 :: BitVector 2

  it "Check ++# behavior" $ do
    -- Test ++# operator: LEFT operand goes to HIGH bits, RIGHT operand goes to LOW bits
    let msg = 0b110011 :: BitVector 6
    let zeros = 0 :: BitVector 10
    -- To get msg in LOW bits, put zeros on LEFT
    let extended = zeros ++# msg :: BitVector 16
    putStrLn $ "msg: " P.++ show msg
    putStrLn $ "extended (low 8 bits): " P.++ show (resize extended :: BitVector 8)
    putStrLn $ "testBit extended 0: " P.++ show (testBit extended 0)
    putStrLn $ "testBit extended 5: " P.++ show (testBit extended 5)
    -- Low 6 bits should match original message
    testBit extended 0 `shouldBe` testBit msg 0
    testBit extended 5 `shouldBe` testBit msg 5

  it "Check message encoding" $ do
    let msgBitString = toBitString $(listToVecTH "abc")
    let msgAsBitVector = pack msgBitString :: BitVector 24
    -- Debug: show actual bits
    putStrLn $ "msgBitString (first 8 bits): " P.++ show (take d8 msgBitString)
    putStrLn $ "msgAsBitVector: " P.++ show msgAsBitVector
    -- "abc" = 0x61 0x62 0x63 in ASCII
    -- SHA3 spec uses little-endian bit ordering within bytes
    -- Bit 0 = LSB of first byte = bit 0 of 'a' (0x61)
    -- But toBitString might use different bit ordering
    putStrLn "Expected would be: 0x636261 for normal ordering"
    -- Don't fail, just observe
    True `shouldBe` True

  it "Check padded blocks" $ do
    let msgBitString = toBitString $(listToVecTH "abc")
    let msgAsBitVector = pack msgBitString :: BitVector 24
    let msgWithSuffix = msgAsBitVector ++# sha3Suffix :: BitVector 26

    putStrLn $ "msgAsBitVector (24 bits): " P.++ show msgAsBitVector
    putStrLn $ "sha3Suffix (2 bits): " P.++ show sha3Suffix
    putStrLn $ "msgWithSuffix (26 bits): " P.++ show msgWithSuffix
    putStrLn $ "testBit msgWithSuffix 0: " P.++ show (testBit msgWithSuffix 0)
    putStrLn $ "testBit msgWithSuffix 24: " P.++ show (testBit msgWithSuffix 24)
    putStrLn $ "testBit msgWithSuffix 25: " P.++ show (testBit msgWithSuffix 25)

    let paddedBlocks = Sponge.Pure.padToRateBlocks @1088 msgWithSuffix

    -- Should have 1 block for short message
    length paddedBlocks `shouldBe` 1

    -- First block should be: msg || suffix || 1 || 0...0 || 1
    -- Bits [25:0] = msg with suffix
    -- Bit 26 = pad start (1)
    -- Bits [1086:27] = zeros
    -- Bit 1087 = pad end (1)
    let firstBlock = head paddedBlocks
    putStrLn $ "First block (low 64 bits): " P.++ show (resize firstBlock :: BitVector 64)
    putStrLn $ "testBit firstBlock 0: " P.++ show (testBit firstBlock 0)
    putStrLn $ "testBit firstBlock 26: " P.++ show (testBit firstBlock 26)

  it "Check suffix appending" $ do
    let msgBitString = toBitString $(listToVecTH "abc")
    -- Reference way: Vec ++
    let msgWithSuffixRefVec = msgBitString ++ unpack (0b01 :: BitVector 2)
    let msgWithSuffixRef = pack msgWithSuffixRefVec :: BitVector 26

    -- My way: BitVector ++#
    let msgAsBitVector = pack msgBitString :: BitVector 24
    let msgWithSuffixMine = sha3Suffix ++# msgAsBitVector :: BitVector 26

    putStrLn $ "Reference msgWithSuffix (low 8 bits): " P.++ show (resize msgWithSuffixRef :: BitVector 8)
    putStrLn $ "My msgWithSuffix (low 8 bits): " P.++ show (resize msgWithSuffixMine :: BitVector 8)
    msgWithSuffixRef `shouldBe` msgWithSuffixMine

  it "Compare first absorption" $ do
    -- Compare what happens after absorbing first block
    let msgBitString = toBitString $(listToVecTH "abc")
    let msgAsBitVector = pack msgBitString :: BitVector 24

    -- Reference: absorb using SHA3internal
    let msgWithSuffixRef = msgBitString ++ unpack (0b01 :: BitVector 2)
    let paddedRef = Sponge.Pure.padToRateBlocks @1088 (pack msgWithSuffixRef :: BitVector 26)
    let firstBlockRef = head paddedRef
    putStrLn $ "Reference first block (low 32 bits): " P.++ show (resize firstBlockRef :: BitVector 32)

    -- My implementation
    let actual = Sponge.Pure.pureSponge @1600 @1088 @256 sha3Suffix Perm.keccakF1600 msgAsBitVector
    putStrLn $ "pureSponge output: " P.++ show (resize actual :: BitVector 32)

  it "SHA3-256 hash of 'abc'" $ do
    -- Get reference output using SHA3.sha3_256 (it handles suffix internally)
    let msgBitString = toBitString $(listToVecTH "abc")
    let expectedBitString = SHA3.sha3_256 msgBitString
    let expected = pack expectedBitString

    -- pureSponge also handles suffix internally, same as SHA3.sha3_256
    let msgAsBitVector = pack msgBitString :: BitVector 24
    let actual = Sponge.Pure.pureSponge @1600 @1088 @256 sha3Suffix Perm.keccakF1600 msgAsBitVector

    expected `shouldBe` 0x3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532
    actual `shouldBe` expected
-- 0x3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532

  -- it "SHA3-256 hash of empty string" $ do
  --   -- Empty message (0 bits)
  --   let msg = 0 :: BitVector 0
  --   let actual = Sponge.Pure.pureSponge @1600 @1088 @256 sha3Suffix Perm.keccakF1600 msg
  --   let expected = 0xa7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a :: BitVector 256
  --   actual `shouldBe` expected
