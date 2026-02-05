{-# LANGUAGE DataKinds #-}

module Test.TestHarness.G512
  ( G512Test,
    g512Gen,
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

type G512Test = Common.GTest

g512Gen :: Gen G512Test
g512Gen = Common.gGen

runTest :: G512Test -> Expectation
runTest = Common.runTest (Common.gParamsFor MLKEM512)

runHardware :: G512Test -> ByteString
runHardware = Common.runHardware (Common.gParamsFor MLKEM512)

testLabel :: G512Test -> String
testLabel = Common.testLabel
