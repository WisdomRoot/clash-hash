{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined.SHA3256Normal (spec) where

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Word (Word8)
import Numeric (showHex)
import Prelude
import Test.Hspec
import Test.QuickCheck
import Test.TestHarness.SHA3256Normal
import Test.TestHarness.SHAKECommon (makeBasicTest, makeCombinedTest, makeStallTest)
import Test.TestHarness.SHAKESamples qualified as Samples

spec :: Spec
spec = describe "NonPipelined SHA3-256 (Normal) Tests" $ do
  let emptyCase = makeBasicTest BS.empty 32
      zeroCase = makeBasicTest (BS.replicate 136 0) 32
      basicCases =
        [ emptyCase,
          makeBasicTest Samples.msg1088 32,
          makeBasicTest Samples.msg2176 32,
          makeBasicTest Samples.msg3264 32
        ]
      stallCases =
        [ makeStallTest Samples.msg1088 32 Samples.stallPatternAggressive
        ]
      backpressureCases =
        [ makeCombinedTest
            Samples.msg1088
            32
            Samples.stallPatternAggressive
            Samples.backpressurePatternAggressive
        ]

  describe "Basic functionality tests" $ do
    it "0-bit (flush-only) input digest (prints)" $ do
      let actual = runHardware emptyCase
      putStrLn ("N256N empty digest: " <> toHex actual)
      runTest emptyCase
  --   it "1088-bit all-zero input digest (prints)" $ do
  --     let actual = runHardware zeroCase
  --     putStrLn ("N256N zero digest: " <> toHex actual)
  --     runTest zeroCase
  --   for_ basicCases $
  --     \testCase -> it (testLabel testCase) $ runTest testCase

  -- describe "Upstream stall handling" $
  --   for_ stallCases $
  --     \testCase -> it (testLabel testCase) $ runTest testCase

  -- describe "Downstream backpressure handling" $
  --   for_ backpressureCases $
  --     \testCase -> it (testLabel testCase) $ runTest testCase

  -- describe "QuickCheck property tests" $
  --   it "correctly handles random test cases" $
  --     withMaxSuccess 10 $
  --       forAll sha3256NormalGen runTest

toHex :: BS.ByteString -> String
toHex bs = concatMap byteToHex (BS.unpack bs)
  where
    byteToHex :: Word8 -> String
    byteToHex b =
      let s = showHex b ""
       in if length s == 1 then '0' : s else s
