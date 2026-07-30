{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.NTT256 (spec) where

import Clash.Prelude (BitVector, Unsigned, Vec, clockGen, enableGen, fromList, resetGen, sampleN)
import Component.NTT qualified as DUT
import MLDSA.NTT qualified as Ref
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, chooseInteger, forAll, vectorOf)
import Prelude qualified as P

-- One (a, b, zeta) triple per cycle; collect the matching outputs.
runDUT :: [(P.Integer, P.Integer, P.Integer)] -> [(BitVector 23, BitVector 23)]
runDUT inputs =
  sampleN
    (P.length inputs)
    (NTT.topEntity clockGen resetGen enableGen (fromList beats))
  where
    beats =
      P.map (\(a, b, z) -> (P.fromInteger a, P.fromInteger b, P.fromInteger z)) inputs
        P.++ P.repeat (0, 0, 0)

it "computes a known butterfly" P.$
  runDUT [(1, 1, 1)] `shouldBe` [(2, 0)]

q :: P.Integer
q = 8_380_417

qInt :: P.Int
qInt = P.fromInteger q

type Coeff = Unsigned 23
type Poly = Vec 256 Coeff
type Zetas = Vec 256 Coeff

genCoeff :: Gen P.Integer
genCoeff = chooseInteger (0, q P.- 1)

genCase :: Gen [(P.Integer, P.Integer, P.Integer)]
genCase = vectorOf 20 ((,,) P.<$> genCoeff P.<*> genCoeff P.<*> genCoeff)

spec :: Spec
spec = describe "NTT" P.$
  it "matches the software model" P.$
    forAll genCase P.$ \inputs ->
      runDUT inputs
        `shouldBe` P.map (\(a, b, z) -> bimapBV (Ref.butterfly a b z)) inputs