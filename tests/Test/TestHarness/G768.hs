{-# LANGUAGE DataKinds #-}

module Test.TestHarness.G768
  ( G768Test,
    g768Gen,
    runTest,
    runHardware,
    testLabel
  )
where

import Data.ByteString (ByteString)
import Parameter (MLKEM (..))
import Test.Hspec (Expectation)
import Test.QuickCheck (Gen)
import Test.TestHarness.G qualified as Common
import Prelude (String)

type G768Test = Common.GTest

g768Gen :: Gen G768Test
g768Gen = Common.gGen

runTest :: G768Test -> Expectation
runTest = Common.runTest (Common.gParamsFor MLKEM768)

runHardware :: G768Test -> ByteString
runHardware = Common.runHardware (Common.gParamsFor MLKEM768)

testLabel :: G768Test -> String
testLabel = Common.testLabel
