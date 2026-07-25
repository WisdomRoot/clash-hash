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

type ButterflyOutput = (Coeff, Coeff)

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

runDUT
  :: [(P.Integer, P.Integer, P.Integer)]
  -> [ButterflyOutput]
runDUT inputs =
  sampleN
    (P.length inputs)
    (NTT.topEntity clockGen resetGen enableGen (fromList beats))
  where
    beats =
      P.map
        (\(a, b, zeta) ->
          ( P.fromInteger a
          , P.fromInteger b
          , P.fromInteger zeta
          )
        )
        inputs
        P.++ P.repeat (0, 0, 0)

toOutput :: (P.Integer, P.Integer) -> ButterflyOutput
toOutput (a, b) =
  (P.fromInteger a, P.fromInteger b)

spec :: Spec
spec =
  describe "Component.NTT butterfly" P.$ do
    it "handles zero inputs" P.$
      runDUT [(0, 0, 0)]
        `shouldBe` [toOutput (0, 0)]

    it "handles zeta = 0" P.$
      runDUT [(5, 7, 0)]
        `shouldBe` [toOutput (5, 5)]

    it "handles zeta = 1" P.$
      runDUT [(5, 7, 1)]
        `shouldBe` [toOutput (12, q P.- 2)]

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
