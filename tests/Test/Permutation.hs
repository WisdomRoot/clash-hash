{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Permutation (spec) where

import Clash.Prelude
import qualified KeccakF1600.Permutation as Perm
import qualified SHA3internal
import Test.Hspec

spec :: Spec
spec = describe "Permutation" $ do
  let sha3Consts = SHA3internal.sha3_constants @6 @64 @1600
  let inputBitVector = 0x0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF :: BitVector 1600
  let input = unpack (0x0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF :: BitVector 1600) :: Vec 1600 Bit

  it "theta" $ do
    let expected = pack $ SHA3internal.theta sha3Consts input
    let actual = pack $ Perm.thetaF1600 input
    actual `shouldBe` expected

  it "rho" $ do
    let expected = pack $ SHA3internal.rho sha3Consts input
    let actual = pack $ Perm.rhoF1600 input
    actual `shouldBe` expected

  it "pi" $ do
    let expected = pack $ SHA3internal.pi sha3Consts input
    let actual = pack $ Perm.piF1600 input
    actual `shouldBe` expected

  it "chi" $ do
    let expected = pack $ SHA3internal.chi sha3Consts input
    let actual = pack $ Perm.chiF1600 input
    actual `shouldBe` expected

  it "iota (round 0)" $ do
    let roundIdx = 0 :: Index 24
    let expected = pack $ SHA3internal.iota sha3Consts roundIdx input
    let actual = pack $ Perm.iotaF1600 roundIdx input
    actual `shouldBe` expected

  it "keccakF1600Round (round 0)" $ do
    let roundIdx = 0 :: Index 24
    let expected = pack $ SHA3internal.keccakf1Round roundIdx input
    let actual = pack $ Perm.keccakF1600Round roundIdx inputBitVector
    actual `shouldBe` expected
