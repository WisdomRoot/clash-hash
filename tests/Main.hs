{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main (main) where

import Test.Combinational qualified
import Test.Constants qualified
import Test.NonPipelined.SHA3256 qualified
import Test.NonPipelined.SHA3256Normal qualified
import Test.NonPipelined.SHA3512 qualified
import Test.NonPipelined.SHA3512Normal qualified
import Test.NonPipelined.G512 qualified
import Test.NonPipelined.G768 qualified
import Test.NonPipelined.G qualified
import Test.G512 qualified
import Test.G qualified
import Test.NonPipelined.SHAKE256 qualified
import Test.NonPipelined.SHAKE128 qualified
import Test.NonPipelined.SHAKE128B qualified
import Test.SampleNTT512 qualified
import Test.Permutation qualified
import Test.PRF2 qualified
import Test.PRF3 qualified
import Test.SamplePolyCBD qualified
import Test.SamplePolyCBD512 qualified
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
  samplePolyCBD512Tests <- testSpec "SamplePolyCBD512" Test.SamplePolyCBD512.spec
  samplePolyCBDStreamTests <- testSpec "SamplePolyCBD Stream" Test.SamplePolyCBD.spec
  combinationalTests <- testSpec "Combinational" Test.Combinational.spec
  n256Tests <- testSpec "NonPipelined SHA3-256" Test.NonPipelined.SHA3256.spec
  n256NormalTests <- testSpec "NonPipelined SHA3-256 Normal" Test.NonPipelined.SHA3256Normal.spec
  n512Tests <- testSpec "NonPipelined SHA3-512" Test.NonPipelined.SHA3512.spec
  n512NormalTests <- testSpec "NonPipelined SHA3-512 Normal" Test.NonPipelined.SHA3512Normal.spec
  gTests <- testSpec "G512" Test.NonPipelined.G512.spec
  gStreamTests <- testSpec "G512 Stream" Test.G512.spec
  g768Tests <- testSpec "G768" Test.NonPipelined.G768.spec
  gGeneralTests <- testSpec "G" Test.NonPipelined.G.spec
  gGeneralStreamTests <- testSpec "G Stream" Test.G.spec
  n256xTests <- testSpec "NonPipelined SHAKE-256" Test.NonPipelined.SHAKE256.spec
  n128xTests <- testSpec "NonPipelined SHAKE-128" Test.NonPipelined.SHAKE128.spec
  n128xbTests <- testSpec "NonPipelined SHAKE-128B" Test.NonPipelined.SHAKE128B.spec
  nSampleNTT512Tests <- testSpec "SampleNTT512" Test.SampleNTT512.spec
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
        samplePolyCBD512Tests,
        samplePolyCBDStreamTests,
        combinationalTests,
        n256Tests,
        n256NormalTests,
        n512Tests,
        n512NormalTests,
        gTests,
        gStreamTests,
        g768Tests,
        gGeneralTests,
        gGeneralStreamTests,
        n256xTests,
        n128xTests,
        n128xbTests,
        refSha3Tests,
        refShake256Tests,
        nSampleNTT512Tests
      ]
