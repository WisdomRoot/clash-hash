{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHAKE256 (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Prelude (($))
import Test.Hspec
import Test.TestCase

spec :: Spec
spec = describe "NonPipelined SHAKE-256 Tests" $ do
  describe "Fixed test cases" $ do
    for_ testCases $ \testCase ->
      it (shakeSimpleLabel testCase) $ runShakeTestCaseSimple testCase

testCases :: [ShakeSimple]
testCases =
  [ ShakeSimple (BS8.pack "qwertyui") 32 NoUpstreamStall NoDownstreamBackpressure,
    ShakeSimple (BS8.pack "qwertyuiopasdfgh") 64 NoUpstreamStall NoDownstreamBackpressure,
    ShakeSimple (BS8.pack msg1024) 32 NoUpstreamStall NoDownstreamBackpressure,
    ShakeSimple (BS8.pack msg1088) 128 NoUpstreamStall NoDownstreamBackpressure,
    ShakeSimple (BS8.pack msg1600) 64 NoUpstreamStall NoDownstreamBackpressure,
    ShakeSimple (BS8.pack msg3200) 256 NoUpstreamStall NoDownstreamBackpressure
  ]
  where
    -- 128 characters = 1024 bits
    msg1024 =
      "qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh\
      \qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh"

    -- 136 characters = 1088 bits
    msg1088 =
      "qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh\
      \qwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfghqwertyuiopasdfgh\
      \qwertyui"

    -- 200 characters = 1600 bits
    msg1600 =
      "01234567890123456789012345678901234567890123456789\
      \01234567890123456789012345678901234567890123456789\
      \01234567890123456789012345678901234567890123456789\
      \01234567890123456789012345678901234567890123456789"

    -- 400 characters = 3200 bits
    msg3200 =
      "01234567890123456789012345678901234567890123456789\
      \01234567890123456789012345678901234567890123456789\
      \01234567890123456789012345678901234567890123456789\
      \01234567890123456789012345678901234567890123456789\
      \01234567890123456789012345678901234567890123456789\
      \01234567890123456789012345678901234567890123456789\
      \01234567890123456789012345678901234567890123456789\
      \01234567890123456789012345678901234567890123456789"
