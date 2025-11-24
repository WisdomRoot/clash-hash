{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Sponge.Properties (spec) where

import Clash.Prelude
import qualified KeccakF1600.Permutation as Perm
import qualified SHA3
import qualified Sponge.Pure as Pure
import Test.Hspec
import qualified Prelude as P

spec :: Spec
spec = describe "Pure Sponge (Phase 1)" $ do
  describe "SHA3-256 test vectors" $ do
    it "64-bit message: all zeros" $ do
      testSHA3_256_64 0x0000000000000000

    it "64-bit message: all ones" $ do
      testSHA3_256_64 0xFFFFFFFFFFFFFFFF

    it "64-bit message: alternating" $ do
      testSHA3_256_64 0xAAAAAAAAAAAAAAAA

    it "64-bit message: counter pattern" $ do
      testSHA3_256_64 0x0001020304050607

    it "64-bit message: single bit" $ do
      testSHA3_256_64 0x8000000000000000

    it "64-bit message: byte pattern" $ do
      testSHA3_256_64 0x0123456789ABCDEF

-- | Test SHA3-256 against software reference for 64-bit messages
testSHA3_256_64 :: BitVector 64 -> Expectation
testSHA3_256_64 msg = do
  -- Reference: software SHA3-256
  let expectedDigest = v2bv (SHA3.sha3_256 (bv2v msg :: Vec 64 Bit))

  -- Hardware: pure sponge with keccakF1600
  let actualDigest = Pure.pureSponge @1600 @1088 @256
                       (0b01 :: BitVector 2)
                       Perm.keccakF1600
                       msg

  -- Compare
  actualDigest `shouldBe` expectedDigest
