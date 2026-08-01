{-# LANGUAGE DataKinds #-}

module Test.NTT256 (spec) where

import Clash.Prelude
import Clash.Sized.Vector qualified as Vec
import Component.NTT qualified as DUT
import Data.Vector qualified as V
import MLDSA.NTT qualified as Ref
import Prelude qualified as P
import Test.Hspec
import Test.QuickCheck

q :: P.Integer
q = 8380417

type Coeff = Unsigned 23
type Poly = Vec 256 Coeff
type Zetas = Vec 256 Coeff

-- Convert an Integer into the canonical range [0, q - 1].
toCoeff :: P.Integer -> Coeff
toCoeff x =
  P.fromInteger (x `P.mod` q)

-- Convert exactly 256 Integer values into the hardware polynomial type.
toPoly :: [P.Integer] -> Poly
toPoly xs
  | P.length xs P.== 256 =
      Vec.unsafeFromList (P.map toCoeff xs)
  | P.otherwise =
      P.error "toPoly: expected exactly 256 coefficients"

rModQInteger :: P.Integer
rModQInteger = 16382

toMontgomeryInteger :: P.Integer -> P.Integer
toMontgomeryInteger x =
  ((x `P.mod` q) P.* rModQInteger) `P.mod` q

-- Convert exactly 256 Integer values into the hardware zeta type.
toZetas :: [P.Integer] -> Zetas
toZetas xs
  | P.length xs P.== 256 =
      Vec.unsafeFromList
        (P.map
          (toCoeff P.. toMontgomeryInteger)
          xs)
  | P.otherwise =
      P.error "toZetas: expected exactly 256 zetas"

-- Drive the real topEntity input ports and collect one combinational output.
runDUT :: [P.Integer] -> [P.Integer] -> [P.Integer]
runDUT zetas input =
  case sampleN 1 outputSignal of
    [output] ->
      P.map P.toInteger (toList output)

    _ ->
      P.error "runDUT: unexpected output sample count"
  where
    inputBeat :: (Zetas, Poly)
    inputBeat =
      (toZetas zetas, toPoly input)

    zeroBeat :: (Zetas, Poly)
    zeroBeat =
      ( toZetas (P.replicate 256 0)
      , toPoly (P.replicate 256 0)
      )

    -- Signals must be infinite, so pad after the real input.
    beats :: [(Zetas, Poly)]
    beats =
      inputBeat : P.repeat zeroBeat

    outputSignal =
      DUT.topEntity
        clockGen
        resetGen
        enableGen
        (fromList beats)

-- Run the software reference implementation.
runReference :: [P.Integer] -> [P.Integer] -> [P.Integer]
runReference zetas input =
  V.toList
    (Ref.ntt
      q
      (V.fromList (P.map normalizeInteger zetas))
      (V.fromList (P.map normalizeInteger input)))
  where
    normalizeInteger :: P.Integer -> P.Integer
    normalizeInteger x =
      x `P.mod` q

genCoeff :: Gen P.Integer
genCoeff =
  chooseInteger (0, q P.- 1)

genPoly :: Gen [P.Integer]
genPoly =
  vectorOf 256 genCoeff

genNTTCase :: Gen ([P.Integer], [P.Integer])
genNTTCase =
  (,)
    P.<$> genPoly
    P.<*> genPoly

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

-- Deterministic test zetas.
-- Both DUT and reference receive the same table.
testZetas :: [P.Integer]
testZetas =
  [ (P.fromIntegral i P.* 1753 P.+ 9271) `P.mod` q
  | i <- [0 :: P.Int .. 255]
  ]

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

                bHardware :: Coeff
                bHardware =
                  toCoeff b

                actual :: P.Integer
                actual =
                  P.toInteger
                    (DUT.montgomeryMul zetaMont bHardware)

                expected :: P.Integer
                expected =
                  (zeta P.* b) `P.mod` q
              in actual `shouldBe` expected

      it "handles Montgomery multiplication boundaries" P.$ do
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
        runDUT testZetas zeroPoly
          `shouldBe` zeroPoly

      it "matches MLDSA.NTT on a ramp polynomial" P.$
        runDUT testZetas rampPoly
          `shouldBe` runReference testZetas rampPoly

      it "matches MLDSA.NTT near coefficient boundaries" P.$
        runDUT testZetas boundaryPoly
          `shouldBe` runReference testZetas boundaryPoly

      it "matches MLDSA.NTT on an alternating polynomial" P.$
        runDUT testZetas alternatingPoly
          `shouldBe` runReference testZetas alternatingPoly

      it "does not consume zetas[0]" P.$ do
        let
          zetasA =
            0 : P.tail testZetas

          zetasB =
            (q P.- 1) : P.tail testZetas

        runDUT zetasA rampPoly
          `shouldBe` runDUT zetasB rampPoly

      it "keeps all output coefficients reduced modulo q" P.$
        withMaxSuccess 5 P.$
          forAll genNTTCase P.$ \(zetas, input) ->
            runDUT zetas input
              `shouldSatisfy` allReduced

      it "matches MLDSA.NTT for random reduced inputs" P.$
        withMaxSuccess 5 P.$
          forAll genNTTCase P.$ \(zetas, input) ->
            runDUT zetas input
              `shouldBe` runReference zetas input