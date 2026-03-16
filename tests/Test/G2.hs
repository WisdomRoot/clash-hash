module Test.G2 (spec) where

import Component.G2 qualified as G2
import Test.G.Specialized (specializedSpec)
import Test.Hspec (Spec)

spec :: Spec
spec = specializedSpec "G2" 2 G2.i256o512
