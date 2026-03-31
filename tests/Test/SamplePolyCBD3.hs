module Test.SamplePolyCBD3
  ( specO12,
  )
where

import Component.PRF.Common (Eta (Eta3))
import Component.SamplePolyCBD3 qualified as SamplePolyCBD3
import Test.Hspec (Spec)
import Test.SamplePolyCBD.FixedEta (fixedEtaSpecO12)

specO12 :: Spec
specO12 = fixedEtaSpecO12 "CBD3-O12" Eta3 SamplePolyCBD3.i264o12
