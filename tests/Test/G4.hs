module Test.G4 (spec) where

import Component.G4 qualified as G4
import Test.G.Specialized (specializedSpec)
import Test.Hspec (Spec)

spec :: Spec
spec = specializedSpec "G4" 4 G4.i256o512
