module Test.PRF3 (spec) where

import Component.PRF3 qualified as PRF3
import Data.ByteString qualified as BS
import Test.Hspec (Spec)
import Component.PRF.Common (Eta (..))
import Test.PRF.Common (PRFConfig (..), prfSpec)

spec :: Spec
spec =
  prfSpec
    PRFConfig
      { pcName = "PRF3",
        pcEta = Eta3,
        pcSeed = BS.pack [31, 30 .. 0],
        pcByte = 0xA5,
        pcTopEntity = PRF3.topEntity
      }
