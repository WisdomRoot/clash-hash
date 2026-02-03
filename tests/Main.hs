{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main (main) where

import Test.Combinational qualified
import Test.Constants qualified
import Test.NonPipelined.SHA3256 qualified
import Test.NonPipelined.SHA3256Normal qualified
import Test.NonPipelined.SHA3512 qualified
import Test.NonPipelined.SHA3512Normal qualified
import Test.NonPipelined.SHA3512NormalG qualified
import Test.NonPipelined.SHAKE256 qualified
import Test.NonPipelined.SHAKE128 qualified
import Test.NonPipelined.SHAKE128B qualified
import Test.SampleNTT qualified
import Test.SampleNTT2 qualified
import Test.Permutation qualified
import Test.PRF2 qualified
import Test.PRF3 qualified
import Test.SamplePolyCBD2 qualified
import Test.SamplePolyCBD3 qualified
import Test.Reference.SHA3 qualified
import Test.Reference.SHAKE256 qualified
import Test.Tasty
import Test.Tasty.Hspec
import Prelude

main :: IO ()
main = do
  constantsTests <- testSpec "Constants" Test.Constants.spec
  permutationRevTests <- testSpec "Permutation" Test.Permutation.spec
  prf2Tests <- testSpec "PRF2" Test.PRF2.spec
  prf3Tests <- testSpec "PRF3" Test.PRF3.spec
  samplePolyCBD2Tests <- testSpec "SamplePolyCBD2" Test.SamplePolyCBD2.spec
  samplePolyCBD3Tests <- testSpec "SamplePolyCBD3" Test.SamplePolyCBD3.spec
  combinationalTests <- testSpec "Combinational" Test.Combinational.spec
  n256Tests <- testSpec "NonPipelined SHA3-256" Test.NonPipelined.SHA3256.spec
  n256NormalTests <- testSpec "NonPipelined SHA3-256 Normal" Test.NonPipelined.SHA3256Normal.spec
  n512Tests <- testSpec "NonPipelined SHA3-512" Test.NonPipelined.SHA3512.spec
  n512NormalTests <- testSpec "NonPipelined SHA3-512 Normal" Test.NonPipelined.SHA3512Normal.spec
  gTests <- testSpec "G" Test.NonPipelined.SHA3512NormalG.spec
  n256xTests <- testSpec "NonPipelined SHAKE-256" Test.NonPipelined.SHAKE256.spec
  n128xTests <- testSpec "NonPipelined SHAKE-128" Test.NonPipelined.SHAKE128.spec
  n128xbTests <- testSpec "NonPipelined SHAKE-128B" Test.NonPipelined.SHAKE128B.spec
  nSampleNTTTests <- testSpec "SampleNTT" Test.SampleNTT.spec
  nSampleNTT2Tests <- testSpec "SampleNTT2" Test.SampleNTT2.spec
  refSha3Tests <- testSpec "Reference SHA3-256" Test.Reference.SHA3.spec
  refShake256Tests <- testSpec "Reference SHAKE-256" Test.Reference.SHAKE256.spec

  defaultMain $
    localOption (mkTimeout 10000000) $  -- 10 second timeout per test
    testGroup
      "All Tests"
      [
        constantsTests,
        permutationRevTests,
        prf2Tests,
        prf3Tests,
        samplePolyCBD2Tests,
        samplePolyCBD3Tests,
        combinationalTests,
        n256Tests,
        n256NormalTests,
        n512Tests,
        n512NormalTests,
        gTests,
        n256xTests,
        n128xTests,
        n128xbTests,
        refSha3Tests,
        refShake256Tests,
        nSampleNTTTests,
        nSampleNTT2Tests
      ]
