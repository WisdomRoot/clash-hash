{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHAKE256 (spec) where

import Clash.Prelude hiding (tlast)
import Data.Foldable (for_)
import Reference.SHA3internal qualified as SHA3internal
import Test.Hspec
import Test.QuickCheck
import Test.TestCase

spec :: Spec
spec = describe "NonPipelined SHAKE-256 Tests" $ do
  describe "Fixed test cases" $ do
    for_ testCases $ \testCase ->
      it (shakeTestCaseLabel testCase) $ runShakeTestCase testCase

-- describe "QuickCheck property tests" $ do
--   it "correctly handles random test cases with upstream stalls" $
--     withMaxSuccess 5 $
--       property $ \ (testCase :: Shake) -> runShakeTestCase testCase

testCases :: [Shake]
testCases =
  [ Shake (ShakeSomeMessage msg64) NoUpstreamStall NoDownstreamBackpressure,
    Shake (ShakeSomeMessage msg128) NoUpstreamStall NoDownstreamBackpressure,
    Shake (ShakeSomeMessage msg1024) NoUpstreamStall NoDownstreamBackpressure,
    Shake (ShakeSomeMessage msg1088) NoUpstreamStall NoDownstreamBackpressure,
    Shake (ShakeSomeMessage msg1600) NoUpstreamStall NoDownstreamBackpressure,
    Shake (ShakeSomeMessage msg3200) NoUpstreamStall NoDownstreamBackpressure
  ]
  where
    msg64 :: Vec (1 * 64) Bit
    msg64 = SHA3internal.toBitString $(listToVecTH "qwertyui")

    msg128 :: Vec (2 * 64) Bit
    msg128 = SHA3internal.toBitString $(listToVecTH "qwertyuiopasdfgh")

    msg1024 :: Vec (16 * 64) Bit
    msg1024 =
      SHA3internal.toBitString
        $(listToVecTH "qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh")

    msg1088 :: Vec (17 * 64) Bit
    msg1088 =
      SHA3internal.toBitString
        $(listToVecTH "qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyui")

    msg1600 :: Vec (25 * 64) Bit
    msg1600 =
      SHA3internal.toBitString
        $(listToVecTH "01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")

    msg3200 :: Vec (50 * 64) Bit
    msg3200 =
      SHA3internal.toBitString
        $(listToVecTH "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")
