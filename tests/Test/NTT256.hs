{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Test.NTT256 (spec) where

import Clash.Prelude
import Clash.Sized.Vector qualified as Vec
import Component.NTT qualified as DUT
import Component.NTTConstants qualified as Constants
import Data.List qualified as List
import Data.Vector qualified as V
import MLDSA.NTT qualified as Ref
import Prelude qualified as P
import Test.Hspec
import Test.QuickCheck

q :: P.Integer
q = 8380417

type Coeff = Unsigned 23
type Poly = Vec 256 Coeff

-- Number of clocks we allow a complete hardware NTT to take.
-- The current 4-lane / 3-stage butterfly design should finish well before this.
maxSimulationCycles :: P.Int
maxSimulationCycles = 512

-- Keep start low long enough for resetGen to be safely deasserted,
-- then pulse it for exactly one clock.
startDelayCycles :: P.Int
startDelayCycles = 3

-- Convert an Integer into the canonical hardware coefficient range [0, q-1].
toCoeff :: P.Integer -> Coeff
toCoeff x =
  P.fromInteger (x `P.mod` q)

-- Convert exactly 256 Integer coefficients into the Clash polynomial type.
toPoly :: [P.Integer] -> Poly
toPoly xs
  | P.length xs P.== 256 =
      Vec.unsafeFromList (P.map toCoeff xs)
  | P.otherwise =
      P.error "toPoly: expected exactly 256 coefficients"

polyToIntegers :: Poly -> [P.Integer]
polyToIntegers =
  P.map P.toInteger P.. toList

normalizeInteger :: P.Integer -> P.Integer
normalizeInteger x =
  x `P.mod` q

-- The DUT now contains zetasMont internally rather than receiving zetas
-- through topEntity. Convert each Montgomery-domain zeta back to the
-- ordinary representation for the software reference NTT.
--
-- montgomeryMul(zetaMont, 1) = zeta (mod q)
referenceZetas :: V.Vector P.Integer
referenceZetas =
  V.fromList
    (P.map
      (\zetaMont ->
        P.toInteger
          (DUT.montgomeryMul zetaMont (1 :: Coeff)))
      (toList Constants.zetasMont))

-- Simulate the actual clocked topEntity.
-- Returns all sampled (done, result) pairs so tests can inspect timing/status.
simulateDUT :: [P.Integer] -> [(P.Bool, Poly)]
simulateDUT input =
  sampleN maxSimulationCycles outputSignal
  where
    inputPoly :: Poly
    inputPoly =
      toPoly input

    startSamples :: [P.Bool]
    startSamples =
      P.replicate startDelayCycles False
        P.++ [True]
        P.++ P.repeat False

    polySamples :: [Poly]
    polySamples =
      P.repeat inputPoly

    (doneSignal, resultSignal) =
      DUT.topEntity
        clockGen
        resetGen
        enableGen
        (fromList startSamples)
        (fromList polySamples)

    outputSignal =
      bundle (doneSignal, resultSignal)

-- Run the DUT and return the polynomial present on the first done pulse.
runDUT :: [P.Integer] -> [P.Integer]
runDUT input =
  case List.find P.fst (simulateDUT input) of
    P.Just (_, result) ->
      polyToIntegers result

    P.Nothing ->
      P.error
        ( "runDUT: DUT did not assert done within "
            P.++ P.show maxSimulationCycles
            P.++ " cycles"
        )

-- Return the sampled cycle number on which done first becomes True.
doneCycle :: [P.Integer] -> P.Maybe P.Int
doneCycle input =
  P.fmap P.fst
    (List.find (P.fst P.. P.snd) numbered)
  where
    numbered =
      P.zip [0 ..] (simulateDUT input)

-- Run the software/reference implementation using the same zeta constants
-- as the hardware.
runReference :: [P.Integer] -> [P.Integer]
runReference input =
  V.toList
    (Ref.ntt
      q
      referenceZetas
      (V.fromList (P.map normalizeInteger input)))

-- -----------------------------------------------------------------------------
-- Test data
-- -----------------------------------------------------------------------------

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

-- -----------------------------------------------------------------------------
-- Tests
-- -----------------------------------------------------------------------------

spec :: Spec
spec =
  describe "Component.NTT pipelined 256-point NTT" P.$ do

    describe "Montgomery multiplication" P.$ do
      it "matches ordinary multiplication when the zeta is in Montgomery form" P.$
        withMaxSuccess 100 P.$
          forAll genCoeff P.$ \zeta ->
            forAll genCoeff P.$ \b ->
              let
                -- R mod q for R = 2^24.
                rModQ :: P.Integer
                rModQ = 16382

                zetaMont :: Coeff
                zetaMont =
                  toCoeff ((zeta P.* rModQ) `P.mod` q)

                actual :: P.Integer
                actual =
                  P.toInteger
                    (DUT.montgomeryMul zetaMont (toCoeff b))

                expected :: P.Integer
                expected =
                  (zeta P.* b) `P.mod` q
              in
                actual `shouldBe` expected

      it "handles Montgomery boundary values" P.$ do
        let
          rModQ :: P.Integer
          rModQ = 16382

          check :: P.Integer -> P.Integer -> Expectation
          check zeta b =
            P.toInteger
              (DUT.montgomeryMul
                (toCoeff ((zeta P.* rModQ) `P.mod` q))
                (toCoeff b))
              `shouldBe`
                ((zeta P.* b) `P.mod` q)

        check 0 0
        check 1 1
        check (q P.- 1) 1
        check 1 (q P.- 1)
        check (q P.- 1) (q P.- 1)

    describe "pipeline/controller behavior" P.$ do
      it "eventually asserts done" P.$
        doneCycle rampPoly `shouldSatisfy` P.maybe False (P.const True)

      it "asserts done exactly once for one start pulse" P.$ do
        let
          doneCount =
            P.length
              (P.filter P.fst (simulateDUT rampPoly))

        doneCount `shouldBe` 1

      it "finishes before the simulation timeout" P.$
        case doneCycle rampPoly of
          P.Just cycleNumber ->
            cycleNumber `shouldSatisfy` (P.< maxSimulationCycles)

          P.Nothing ->
            expectationFailure "NTT never asserted done"

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

      it "keeps every output coefficient reduced modulo q" P.$
        withMaxSuccess 3 P.$
          forAll genPoly P.$ \input ->
            runDUT input
              `shouldSatisfy` allReduced

      it "matches MLDSA.NTT for random reduced inputs" P.$
        withMaxSuccess 3 P.$
          forAll genPoly P.$ \input ->
            runDUT input
              `shouldBe` runReference input