{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Test.NTT (spec) where

import Clash.Prelude
  ( Unsigned
  , clockGen
  , enableGen
  , fromList
  , resetGen
  , sampleN
  )
import Component.NTT qualified as NTT
import Prelude qualified as P
import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  )

q :: P.Integer
q = 8380417

type Coeff = Unsigned 23

type ButterflyInput =
  (P.Integer, P.Integer, P.Integer)

type HardwareInput =
  (Coeff, Coeff, Coeff)

type ButterflyOutput =
  (Coeff, Coeff)

type DualButterflyInput =
  (HardwareInput, HardwareInput)

type DualButterflyOutput =
  (ButterflyOutput, ButterflyOutput)

refButterfly
  :: P.Integer
  -> P.Integer
  -> P.Integer
  -> (P.Integer, P.Integer)
refButterfly a b zeta =
  let t = (zeta P.* b) `P.mod` q
      outA = (a P.+ t) `P.mod` q
      outB = (a P.- t) `P.mod` q
   in (outA, outB)

toHardwareInput :: ButterflyInput -> HardwareInput
toHardwareInput (a, b, zeta) =
  ( P.fromInteger a
  , P.fromInteger b
  , P.fromInteger zeta
  )

toOutput :: (P.Integer, P.Integer) -> ButterflyOutput
toOutput (a, b) =
  (P.fromInteger a, P.fromInteger b)

-- Pack two butterfly operations into one clock-cycle input.
-- When the number of inputs is odd, the final unused lane is zero-filled.
pairInputs :: [HardwareInput] -> [DualButterflyInput]
pairInputs [] =
  []

pairInputs [input0] =
  [(input0, (0, 0, 0))]

pairInputs (input0 : input1 : remainingInputs) =
  (input0, input1) : pairInputs remainingInputs

flattenOutput :: DualButterflyOutput -> [ButterflyOutput]
flattenOutput (output0, output1) =
  [output0, output1]

runDUT
  :: [ButterflyInput]
  -> [ButterflyOutput]
runDUT inputs =
  P.take (P.length inputs) P.$
    P.concatMap flattenOutput sampledOutputs
  where
    hardwareInputs :: [HardwareInput]
    hardwareInputs =
      P.map toHardwareInput inputs

    beats :: [DualButterflyInput]
    beats =
      pairInputs hardwareInputs
        P.++ P.repeat
          ( (0, 0, 0)
          , (0, 0, 0)
          )

    cycleCount :: P.Int
    cycleCount =
      (P.length inputs P.+ 1) `P.div` 2

    sampledOutputs :: [DualButterflyOutput]
    sampledOutputs =
      sampleN
        cycleCount
        ( NTT.topEntity
            clockGen
            resetGen
            enableGen
            (fromList beats)
        )

spec :: Spec
spec =
  describe "Component.NTT dual butterfly" P.$ do
    it "handles zero inputs" P.$
      runDUT [(0, 0, 0)]
        `shouldBe` [toOutput (0, 0)]

    it "handles zeta = 0" P.$
      runDUT [(5, 7, 0)]
        `shouldBe` [toOutput (5, 5)]

    it "handles zeta = 1" P.$
      runDUT [(5, 7, 1)]
        `shouldBe` [toOutput (12, q P.- 2)]

    it "processes two butterflies in one cycle" P.$ do
      let vectors =
            [ (5, 7, 1)
            , (10, 3, 2)
            ]

          expected =
            P.map
              (\(a, b, zeta) ->
                toOutput (refButterfly a b zeta)
              )
              vectors

      runDUT vectors `shouldBe` expected

    it "handles an odd number of butterfly inputs" P.$ do
      let vectors =
            [ (5, 7, 1)
            , (10, 3, 2)
            , (20, 4, 3)
            ]

          expected =
            P.map
              (\(a, b, zeta) ->
                toOutput (refButterfly a b zeta)
              )
              vectors

      runDUT vectors `shouldBe` expected

    it "matches the reference butterfly" P.$ do
      let vectors =
            [ (0, 0, 0)
            , (1, 1, 1)
            , (5, 7, 1)
            , (q P.- 1, 1, 1)
            , (1, q P.- 1, 1)
            , (123456, 654321, 1753)
            , (q P.- 1, q P.- 1, q P.- 1)
            ]

          expected =
            P.map
              (\(a, b, zeta) ->
                toOutput (refButterfly a b zeta)
              )
              vectors

      runDUT vectors `shouldBe` expected
