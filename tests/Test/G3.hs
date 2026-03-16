module Test.G3 (spec) where

import Component.G3 qualified as G3
import Test.G.Specialized (specializedSpec)
import Test.Hspec (Spec)

spec :: Spec
spec = specializedSpec "G3" 3 G3.i256o512
