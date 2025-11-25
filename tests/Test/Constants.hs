{-# LANGUAGE TemplateHaskell #-}

module Test.Constants (spec) where

import Clash.Prelude
import qualified Prelude as P
import Test.Hspec
import qualified SHA3internal
import qualified Constants

spec :: Spec
spec = describe "Constants" $ do
  it "iota round constants match reference" $ do
    let hwConstants = $(Constants.iota) :: Vec 24 (Vec 64 Bit)
    let refConstants = SHA3internal._iota_constants :: Vec 24 (Vec 64 Bit)

    let allMatch = P.all (\(idx :: Int) -> hwConstants !! idx P.== refConstants !! idx) [0..23]
    if allMatch
      then P.putStrLn "\n✓ All 24 round constants match!"
      else expectationFailure "Round constants mismatch"
