module Main (main) where

import Test.Combinational qualified
import Test.Constants qualified
import Test.SHA3256 qualified
import Test.SHA3512 qualified
import Test.G2 qualified
import Test.G3 qualified
import Test.G4 qualified
import Test.G qualified
import Test.GX2 qualified
import Test.GX3 qualified
import Test.GX4 qualified
import Test.GX6 qualified
import Test.GX8 qualified
import Test.Lookahead4 qualified
import Test.XOF qualified
import Test.XOF6 qualified
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
  cbdo24Tests <- testSpec "CBD-O24" Test.SamplePolyCBD.specO24
  cbd2o12Tests <- testSpec "CBD2-O12" Test.SamplePolyCBD2.specO12
  cbd3o12Tests <- testSpec "CBD3-O12" Test.SamplePolyCBD3.specO12
  combinationalTests <- testSpec "Combinational" Test.Combinational.spec
  sha3256Tests <- testSpec "SHA3-256" Test.SHA3256.spec
  sha3512Tests <- testSpec "SHA3-512" Test.SHA3512.spec
  g2Tests <- testSpec "G2" Test.G2.spec
  g3Tests <- testSpec "G3" Test.G3.spec
  g4Tests <- testSpec "G4" Test.G4.spec
  gTests <- testSpec "G" Test.G.spec
  gx2Tests <- testSpec "G-X2" Test.GX2.spec
  gx3Tests <- testSpec "G-X3" Test.GX3.spec
  gx4Tests <- testSpec "G-X4" Test.GX4.spec
  gx6Tests <- testSpec "G-X6" Test.GX6.spec
  gx8Tests <- testSpec "G-X8" Test.GX8.spec
  xofTests <- testSpec "XOF" Test.XOF.spec
  xof6Tests <- testSpec "XOF6" Test.XOF6.spec
  lookahead4Tests <- testSpec "lookahead4" Test.Lookahead4.spec
  shake3256Tests <- testSpec "SHAKE3-256" Test.SHAKE256.spec
  shake3128Tests <- testSpec "SHAKE3-128" Test.SHAKE128.spec
  snO24L2Tests <- testSpec "SN-O24-L2" Test.SampleNTT.specL2
  snO24L4Tests <- testSpec "SN-O24-L4" Test.SampleNTT.specL4
  snO24L6Tests <- testSpec "SN-O24-L6" Test.SampleNTT.specL6
  snO48L6Tests <- testSpec "SN-O48-L6" Test.SampleNTT.specL6O48
  refSha3Tests <- testSpec "Reference SHA3-256" Test.Reference.SHA3.spec
  refShake256Tests <- testSpec "Reference SHAKE-256" Test.Reference.SHAKE256.spec

  defaultMain $
    localOption (mkTimeout 60000000) $  -- 60 second timeout per test
    testGroup
      "All Tests"
      [
        constantsTests,
        permutationRevTests,
        cbdo24Tests,
        cbd2o12Tests,
        cbd3o12Tests,
        combinationalTests,
        sha3256Tests,
        sha3512Tests,
        g2Tests,
        g3Tests,
        g4Tests,
        gTests,
        gx2Tests,
        gx3Tests,
        gx4Tests,
        gx6Tests,
        gx8Tests,
        xofTests,
        xof6Tests,
        lookahead4Tests,
        shake3256Tests,
        shake3128Tests,
        refSha3Tests,
        refShake256Tests,
        snO24L2Tests,
        snO24L4Tests,
        snO24L6Tests,
        snO48L6Tests
      ]
