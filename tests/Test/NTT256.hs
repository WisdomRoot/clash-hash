{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Test.NTT256 (spec) where

import Clash.Prelude
import Clash.Sized.Vector qualified as Vec
import Component.NTT qualified as DUT
import Component.NTTConstants qualified as Constants
import Data.Vector qualified as V
import MLDSA.NTT qualified as Ref
import Prelude qualified as P
import Test.Hspec
import Test.QuickCheck

q :: P.Integer
q = 8380417

type Coeff = Unsigned 23
type Poly = Vec 256 Coeff

-- Convert an Integer into the canonical range [0, q - 1].
toCoeff :: P.Integer -> Coeff
toCoeff x =
  P.fromInteger (x `P.mod` q)

-- Convert exactly 256 integers into a hardware polynomial.
toPoly :: [P.Integer] -> Poly
toPoly xs
  | P.length xs P.== 256 =
      Vec.unsafeFromList (P.map toCoeff xs)
  | P.otherwise =
      P.error "toPoly: expected exactly 256 coefficients"

-- R mod q, where R = 2^24.
rModQInteger :: P.Integer
rModQInteger = 16382

-- Convert an ordinary value into Montgomery representation.
toMontgomeryInteger :: P.Integer -> P.Integer
toMontgomeryInteger x =
  ((x `P.mod` q) P.* rModQInteger) `P.mod` q

-- Drive the real topEntity and collect one combinational output.
runDUT :: [P.Integer] -> [P.Integer]
runDUT input =
  case sampleN 1 outputSignal of
    [output] ->
      P.map P.toInteger (toList output)

    _ ->
      P.error "runDUT: unexpected output sample count"
  where
    inputBeat :: Poly
    inputBeat =
      toPoly input

    zeroBeat :: Poly
    zeroBeat =
      toPoly (P.replicate 256 0)

    beats :: [Poly]
    beats =
      inputBeat : P.repeat zeroBeat

    outputSignal =
      DUT.topEntity
        clockGen
        resetGen
        enableGen
        (fromList beats)

referenceZetas :: V.Vector P.Integer
referenceZetas =
  V.fromList
    (P.map P.toInteger
      (toList Constants.ordinaryZetas))

-- Run the software reference using the same fixed ordinary zeta table.
runReference :: [P.Integer] -> [P.Integer]
runReference input =
  V.toList
    (Ref.ntt
      q
      referenceZetas
      (V.fromList (P.map normalize input)))
  where
    normalize x = x `P.mod` q

genCoeff :: Gen P.Integer
genCoeff =
  chooseInteger (0, q P.- 1)

genPoly :: Gen [P.Integer]
genPoly =
  vectorOf 256 genCoeff

zeroPoly :: [P.Integer]
zeroPoly =
  P.replicate 256 0

rampPoly :: [P.Integer]
rampPoly =
  P.map P.fromIntegral [0 :: P.Int .. 255]

boundaryPoly :: [P.Integer]
boundaryPoly =
  P.take 256
    (P.cycle [0, 1, q P.- 2, q P.- 1])

alternatingPoly :: [P.Integer]
alternatingPoly =
  P.take 256
    (P.cycle [123456, 654321])

allReduced :: [P.Integer] -> P.Bool
allReduced =
  P.all (\x -> 0 P.<= x P.&& x P.< q)

spec :: Spec
spec =
  describe "Component.NTT" P.$ do
    describe "Montgomery multiplication" P.$ do
      it "produces ordinary zeta times coefficient" P.$
        withMaxSuccess 100 P.$
          forAll genCoeff P.$ \zeta ->
            forAll genCoeff P.$ \b ->
              let
                zetaMont :: Coeff
                zetaMont =
                  toCoeff (toMontgomeryInteger zeta)

                actual :: P.Integer
                actual =
                  P.toInteger
                    (DUT.montgomeryMul
                      zetaMont
                      (toCoeff b))

                expected :: P.Integer
                expected =
                  (zeta P.* b) `P.mod` q
              in
                actual `shouldBe` expected

      it "handles boundary values" P.$ do
        let
          check :: P.Integer -> P.Integer -> Expectation
          check zeta b =
            P.toInteger
              (DUT.montgomeryMul
                (toCoeff (toMontgomeryInteger zeta))
                (toCoeff b))
              `shouldBe`
                ((zeta P.* b) `P.mod` q)

        check 0 0
        check 1 1
        check (q P.- 1) 1
        check 1 (q P.- 1)
        check (q P.- 1) (q P.- 1)

    describe "full 256-point transform" P.$ do
      it "maps the zero polynomial to zero" P.$
        runDUT zeroPoly
          `shouldBe` zeroPoly

      it "matches MLDSA.NTT on a ramp polynomial" P.$
        runDUT rampPoly
          `shouldBe` runReference rampPoly

      it "matches MLDSA.NTT near coefficient boundaries" P.$
        runDUT boundaryPoly
          `shouldBe` runReference boundaryPoly

      it "matches MLDSA.NTT on an alternating polynomial" P.$
        runDUT alternatingPoly
          `shouldBe` runReference alternatingPoly

      it "keeps all output coefficients reduced modulo q" P.$
        withMaxSuccess 5 P.$
          forAll genPoly P.$ \input ->
            runDUT input
              `shouldSatisfy` allReduced

      it "matches MLDSA.NTT for random reduced inputs" P.$
        withMaxSuccess 5 P.$
          forAll genPoly P.$ \input ->
            runDUT input
              `shouldBe` runReference input