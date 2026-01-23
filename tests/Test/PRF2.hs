module Test.PRF2 (spec) where

import Component.PRF2 qualified as PRF2
import Data.ByteString qualified as BS
import Test.Hspec (Spec)
import Component.PRF.Common (Eta (..))
import Test.PRF.Common (PRFConfig (..), prfSpec)

spec :: Spec
spec =
  prfSpec
    PRFConfig
      { pcName = "PRF2",
        pcEta = Eta2,
        pcSeed = BS.pack [0 .. 31],
        pcByte = 0x01,
        pcTopEntity = PRF2.topEntity
      }
