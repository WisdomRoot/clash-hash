{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.G (spec) where

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Word (Word8)
import Prelude
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.GGeneral
import Test.TestHarness.SHAKECommon (makeBasicTest, makeBackpressureTest, makeCombinedTest, makeStallTest)
import Test.TestHarness.SHAKESamples qualified as Samples

basicCases :: [GGeneralTest]
basicCases =
  [ makeBasicTest (msg33 0x00 0x02) 32,
    makeBasicTest (msg33 0x61 0x03) 32,
    makeBasicTest (msg33 0xFF 0x04) 32
  ]

stallCases :: [GGeneralTest]
stallCases =
  [ makeStallTest (msg33 0x7E 0x02) 32 Samples.stallPatternAggressive,
    makeStallTest (msg33 0x33 0x03) 32 Samples.stallPatternModerate,
    makeStallTest (msg33 0xAA 0x04) 32 Samples.stallPatternSimple
  ]

backpressureCases :: [GGeneralTest]
backpressureCases =
  [ makeBackpressureTest (msg33 0x10 0x02) 32 Samples.backpressurePatternSimple,
    makeBackpressureTest (msg33 0x22 0x03) 32 Samples.backpressurePatternModerate,
    makeCombinedTest
      (msg33 0x55 0x04)
      32
      Samples.stallPatternAggressive
      Samples.backpressurePatternAggressive,
    makeCombinedTest
      (msg33 0x99 0xFF)
      32
      Samples.stallPatternModerate
      Samples.backpressurePatternSimple
  ]

msg33 :: Word8 -> Word8 -> BS.ByteString
msg33 fill k = BS.replicate 32 fill <> BS.pack [k]

spec :: Spec
spec = describe "Component G (264-bit input, explicit k) Tests" $ do
  describe "Basic functionality tests" $
    for_ basicCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "Upstream stall handling" $
    for_ stallCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "Downstream backpressure handling" $
    for_ backpressureCases $
      \testCase -> it (testLabel testCase) $ runTest testCase

  describe "QuickCheck property tests" $
    it "correctly handles random test cases" $
      withMaxSuccess 10 $
        forAll gGeneralGen runTest

