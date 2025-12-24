{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined (spec) where

import Clash.Prelude hiding (tlast)
import Data.Foldable (for_)
import Reference.SHA3 qualified as SHA3
import Reference.SHA3internal qualified as SHA3internal
import Test.Hspec
import Test.TestCase
import Test.QuickCheck

run :: IO ()
run = hspec spec

spec :: Spec
spec = describe "NonPipelined SHA3-256 Tests" $ do
  describe "Fixed test cases" $ do
    for_ testCases $ \testCase ->
      it (testCaseLabel testCase) $ runTestCase testCase

  describe "QuickCheck property tests" $ do
    it "correctly handles random test cases with upstream stalls" $
      withMaxSuccess 5 $
        property $ \testCase -> runTestCase testCase

testCases :: [TestCase]
testCases =
  [
    TestCase (SomeMessage msg64) NoUpstreamStall NoDownstreamBackpressure,
    TestCase (SomeMessage msg128) NoUpstreamStall NoDownstreamBackpressure,
    TestCase (SomeMessage msg1024) NoUpstreamStall NoDownstreamBackpressure,
    TestCase (SomeMessage msg1088) NoUpstreamStall NoDownstreamBackpressure,
    TestCase (SomeMessage msg1600) NoUpstreamStall NoDownstreamBackpressure,
    TestCase (SomeMessage msg3200) NoUpstreamStall NoDownstreamBackpressure
  ]
  where
    msg64 :: Vec (1 * 64) Bit
    msg64 = SHA3internal.toBitString $(listToVecTH "qwertyui")
    expected64 :: Vec 4 (BitVector 64)
    expected64 = bitCoerce (SHA3.sha3_256 msg64)

    msg128 :: Vec (2 * 64) Bit
    msg128 = SHA3internal.toBitString $(listToVecTH "qwertyuiopasdfgh")
    expected128 :: Vec 4 (BitVector 64)
    expected128 = bitCoerce (SHA3.sha3_256 msg128)

    msg1024 :: Vec (16 * 64) Bit
    msg1024 =
      SHA3internal.toBitString
        $(listToVecTH "qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh")
    expected1024 :: Vec 4 (BitVector 64)
    expected1024 = bitCoerce (SHA3.sha3_256 msg1024)

    msg1088 :: Vec (17 * 64) Bit
    msg1088 =
      SHA3internal.toBitString
        $(listToVecTH "qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyui")
    expected1088 :: Vec 4 (BitVector 64)
    expected1088 = bitCoerce (SHA3.sha3_256 msg1088)

    msg1600 :: Vec (25 * 64) Bit
    msg1600 =
      SHA3internal.toBitString
        $(listToVecTH "01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")
    expected1600 :: Vec 4 (BitVector 64)
    expected1600 = bitCoerce (SHA3.sha3_256 msg1600)

    msg3200 :: Vec (50 * 64) Bit
    msg3200 =
      SHA3internal.toBitString
        $(listToVecTH "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")
    expected3200 :: Vec 4 (BitVector 64)
    expected3200 = bitCoerce (SHA3.sha3_256 msg3200)
    stallPattern :: [Bool]
    stallPattern =
      [ True,
        False,
        True,
        True,
        False,
        True,
        True,
        True,
        False,
        True,
        True,
        False,
        True,
        True,
        True
      ]
