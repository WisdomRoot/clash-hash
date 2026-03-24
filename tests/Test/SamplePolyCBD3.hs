module Test.SamplePolyCBD3
  ( specO12,
    specO24,
    specO24Dev,
  )
where

import Component.PRF.Common (Eta (Eta3))
import Component.SamplePolyCBD3 qualified as SamplePolyCBD3
import Component.SamplePolyCBD3Dev qualified as SamplePolyCBD3Dev
import Test.Hspec (Spec)
import Test.SamplePolyCBD.FixedEta (fixedEtaSpecO12, fixedEtaSpecO24)

specO12 :: Spec
specO12 = fixedEtaSpecO12 "CBD3-O12" Eta3 SamplePolyCBD3.i264o12

specO24 :: Spec
specO24 = fixedEtaSpecO24 "CBD3-O24" Eta3 SamplePolyCBD3.i264o24

specO24Dev :: Spec
specO24Dev = fixedEtaSpecO24 "CBD3-O24-dev" Eta3 SamplePolyCBD3Dev.i264o24
