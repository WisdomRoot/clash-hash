{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.NTT (spec) where

import Clash.Prelude (BitVector, clockGen, enableGen, fromList, resetGen, sampleN)
import Component.NTT qualified as NTT
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude qualified as P

-- | The ML-DSA (Dilithium) modulus.
q :: P.Integer
q = 8380417

-- | Reference model the DUT must match: add two coefficients modulo 'q'.
refAddModQ :: P.Integer -> P.Integer -> P.Integer
refAddModQ a b = (a P.+ b) `P.mod` q

-- | Drive the (purely combinational) DUT with one @(a, b)@ pair per cycle and
-- collect the matching outputs. Inputs are padded so 'sampleN' never runs off
-- the end of the list.
runDUT :: [(P.Integer, P.Integer)] -> [BitVector 23]
runDUT pairs =
  sampleN
    (P.length pairs)
    (NTT.topEntity clockGen resetGen enableGen (fromList beats))
  where
    beats =
      P.map (\(a, b) -> (P.fromInteger a, P.fromInteger b)) pairs
        P.++ P.repeat (0, 0)

spec :: Spec
spec = describe "Component.NTT (scaffold: a + b mod q)" P.$ do
  it "computes 1 + 1 = 2" P.$
    runDUT [(1, 1)] `shouldBe` [2]

  it "matches the reference a + b mod q on sample vectors" P.$ do
    let vectors =
          [ (0, 0), -- identity
            (1, 1), -- the trivial sanity case
            (5, 7), -- no reduction needed
            (q P.- 1, 1), -- sum == q, wraps to 0
            (q P.- 1, q P.- 1), -- sum == 2q - 2, wraps to q - 2
            (4194304, 4194304) -- sum > q, single subtraction
          ]
        expected =
          P.map (\(a, b) -> P.fromInteger (refAddModQ a b) :: BitVector 23) vectors
    runDUT vectors `shouldBe` expected
