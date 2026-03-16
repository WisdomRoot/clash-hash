{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main (main) where

import Test.Combinational qualified
import Test.Constants qualified
import Test.SHA3256 qualified
import Test.SHA3512 qualified
import Test.G2 qualified
import Test.G3 qualified
import Test.G4 qualified
import Test.G qualified
import Test.XOF qualified
import Test.SHAKE256 qualified
import Test.SHAKE128 qualified
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
  g2Tests <- testSpec "G2" Test.G2.spec
  g3Tests <- testSpec "G3" Test.G3.spec
  g4Tests <- testSpec "G4" Test.G4.spec
  gTests <- testSpec "G" Test.G.spec
  xofTests <- testSpec "XOF" Test.XOF.spec
  shake3256Tests <- testSpec "SHAKE3-256" Test.SHAKE256.spec
  shake3128Tests <- testSpec "SHAKE3-128" Test.SHAKE128.spec
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
        g2Tests,
        g3Tests,
        g4Tests,
        gTests,
        xofTests,
        shake3256Tests,
        shake3128Tests,
        refSha3Tests,
        refShake256Tests,
        snO24L2Tests
      ]
