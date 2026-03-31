module Test.SamplePolyCBD2
  ( specO12,
  )
where

import Component.PRF.Common (Eta (Eta2))
import Component.SamplePolyCBD2 qualified as SamplePolyCBD2
import Test.Hspec (Spec)
import Test.SamplePolyCBD.FixedEta (fixedEtaSpecO12)

specO12 :: Spec
specO12 = fixedEtaSpecO12 "CBD2-O12" Eta2 SamplePolyCBD2.i264o12
