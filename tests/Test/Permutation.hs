{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Permutation (spec) where

import Clash.Prelude
import Permutation qualified
import Reference.SHA3 qualified as SHA3
import Reference.SHA3internal qualified as SHA3internal
import Test.Hspec

spec :: Spec
spec = describe "Permutation" $ do
  let sha3Consts = SHA3internal.sha3_constants @6 @64 @1600
  let inputBitVector = 0x0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF :: BitVector 1600
  let input = unpack (0x0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF :: BitVector 1600) :: Vec 1600 Bit

  describe "PermRev" $ do
    it "theta" $ do
      let expected = pack $ SHA3internal.theta sha3Consts input
      let actual = pack $ Permutation.thetaF1600Reversed input
      actual `shouldBe` expected

    it "rho" $ do
      let expected = pack $ SHA3internal.rho sha3Consts input
      let actual = pack $ Permutation.rhoF1600Reversed input
      actual `shouldBe` expected

    it "pi" $ do
      let expected = pack $ SHA3internal.pi sha3Consts input
      let actual = pack $ Permutation.piF1600Reversed input
      actual `shouldBe` expected

    it "chi" $ do
      let expected = pack $ SHA3internal.chi sha3Consts input
      let actual = pack $ Permutation.chiF1600Reversed input
      actual `shouldBe` expected

    it "iota (round 0)" $ do
      let roundIdx = 0 :: Index 24
      let expected = pack $ SHA3internal.iota sha3Consts roundIdx input
      let actual = pack $ Permutation.iotaF1600Reversed roundIdx (pack input)
      actual `shouldBe` expected

    it "1 round (round 0)" $ do
      let roundIdx = 0 :: Index 24
      let expected = pack $ SHA3internal.keccakf1Round roundIdx input
      let actual = pack $ Permutation.keccakF1600Reversed roundIdx inputBitVector
      actual `shouldBe` expected

    it "24 complete rounds" $ do
      let expected = pack $ SHA3.keccakf @6 @64 @1600 input
      let actual = Permutation.keccakF1600Reversed24 inputBitVector
      actual `shouldBe` expected
