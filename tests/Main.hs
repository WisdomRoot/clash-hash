{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main (main) where

import Test.Combinational qualified
import Test.Constants qualified
import Test.NonPipelined.SHA3256 qualified
import Test.NonPipelined.SHAKE256 qualified
import Test.NonPipelined.SHAKE128 qualified
import Test.Permutation qualified
import Test.Reference.SHA3 qualified
import Test.Reference.SHAKE256 qualified
import Test.Reference.SampleNTT qualified
import Test.Tasty
import Test.Tasty.Hspec
import Prelude

main :: IO ()
main = do
  constantsTests <- testSpec "Constants" Test.Constants.spec
  permutationTests <- testSpec "Permutation" Test.Permutation.spec
  combinationalTests <- testSpec "Combinational" Test.Combinational.spec
  n256Tests <- testSpec "NonPipelined SHA3-256" Test.NonPipelined.SHA3256.spec
  n256xTests <- testSpec "NonPipelined SHAKE-256" Test.NonPipelined.SHAKE256.spec
  n128xTests <- testSpec "NonPipelined SHAKE-128" Test.NonPipelined.SHAKE128.spec
  refSha3Tests <- testSpec "Reference SHA3-256" Test.Reference.SHA3.spec
  refShake256Tests <- testSpec "Reference SHAKE-256" Test.Reference.SHAKE256.spec
  refSampleNTTTests <- testSpec "Reference SampleNTT" Test.Reference.SampleNTT.spec

  defaultMain $
    testGroup
      "All Tests"
      [
        constantsTests,
        permutationTests,
        combinationalTests,
        n256Tests,
        n256xTests,
        n128xTests,
        refSha3Tests,
        refShake256Tests,
        refSampleNTTTests
      ]
