{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main (main) where

import Test.Combinational qualified
import Test.Constants qualified
import Test.SHA3256 qualified
import Test.SHA3512 qualified
import Test.G512 qualified
import Test.G768 qualified
import Test.G qualified
import Test.XOF qualified
import Test.SHAKE256 qualified
import Test.NonPipelined.SHAKE128 qualified
import Test.NonPipelined.SHAKE128B qualified
import Test.SampleNTT qualified
import Test.Permutation qualified
import Test.SamplePolyCBD qualified
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
  cbdo12Tests <- testSpec "CBD-O12" Test.SamplePolyCBD.specO12
  cbdo24Tests <- testSpec "CBD-O24" Test.SamplePolyCBD.specO24
  cbd2o12Tests <- testSpec "CBD2-O12" Test.SamplePolyCBD2.specO12
  cbd2o24Tests <- testSpec "CBD2-O24" Test.SamplePolyCBD2.specO24
  cbd3o12Tests <- testSpec "CBD3-O12" Test.SamplePolyCBD3.specO12
  cbd3o24Tests <- testSpec "CBD3-O24" Test.SamplePolyCBD3.specO24
  combinationalTests <- testSpec "Combinational" Test.Combinational.spec
  sha3256Tests <- testSpec "SHA3-256" Test.SHA3256.spec
  sha3512Tests <- testSpec "SHA3-512" Test.SHA3512.spec
  g512Tests <- testSpec "G512" Test.G512.spec
  g768Tests <- testSpec "G768" Test.G768.spec
  gTests <- testSpec "G" Test.G.spec
  xofTests <- testSpec "XOF" Test.XOF.spec
  shake3256Tests <- testSpec "SHAKE3-256" Test.SHAKE256.spec
  n128xTests <- testSpec "NonPipelined SHAKE-128" Test.NonPipelined.SHAKE128.spec
  n128xbTests <- testSpec "NonPipelined SHAKE-128B" Test.NonPipelined.SHAKE128B.spec
  snO24L2Tests <- testSpec "SN-O24-L2" Test.SampleNTT.spec
  refSha3Tests <- testSpec "Reference SHA3-256" Test.Reference.SHA3.spec
  refShake256Tests <- testSpec "Reference SHAKE-256" Test.Reference.SHAKE256.spec

  defaultMain $
    localOption (mkTimeout 10000000) $  -- 10 second timeout per test
    testGroup
      "All Tests"
      [
        constantsTests,
        permutationRevTests,
        cbdo12Tests,
        cbdo24Tests,
        cbd2o12Tests,
        cbd2o24Tests,
        cbd3o12Tests,
        cbd3o24Tests,
        combinationalTests,
        sha3256Tests,
        sha3512Tests,
        g512Tests,
        g768Tests,
        gTests,
        xofTests,
        shake3256Tests,
        n128xTests,
        n128xbTests,
        refSha3Tests,
        refShake256Tests,
        snO24L2Tests
      ]
