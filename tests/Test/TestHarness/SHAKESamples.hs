{-# LANGUAGE OverloadedStrings #-}

module Test.TestHarness.SHAKESamples
  ( msg576,
    msg1024,
    msg1088,
    msg1152,
    msg1344,
    msg1408,
    msg1600,
    msg2016,
    msg2176,
    msg2688,
    msg3200,
    msg3264,
    stallPatternSimple,
    stallPatternModerate,
    stallPatternAggressive,
    backpressurePatternSimple,
    backpressurePatternModerate,
    backpressurePatternAggressive,
    shake128Gen,
    shake256Gen,
    sha3BasicCases,
    sha3StallCases,
    sha3BackpressureCases,
    sha3512BasicCases,
    sha3512StallCases,
    sha3512BackpressureCases,
    basicCases,
    variableOutputCases,
    stallCases,
    backpressureCases
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Test.QuickCheck (Gen)
import Test.TestHarness.SHAKECommon
  ( ShakeTest,
    genShakeTest,
    makeBackpressureTest,
    makeBasicTest,
    makeCombinedTest,
    makeStallTest,
    makeVariableOutputTest,
    shake128GenConfig,
    shake256GenConfig
  )
import Prelude

--------------------------------------------------------------------------------
-- Shared message fixtures
--------------------------------------------------------------------------------

-- IMPORTANT: All test message inputs MUST be multiples of 8 bytes (64 bits)
-- due to hardware limitation in domain separator placement. The hardware
-- assumes complete 64-bit beats and places the SHAKE domain separator
-- at fixed positions based on beat counter, not actual message bit length.

msg1024 :: ByteString
msg1024 = BS8.replicate (16 * 8) 'q'

msg576 :: ByteString
msg576 = BS8.replicate (9 * 8) 'z'

msg1088 :: ByteString
msg1088 = BS8.replicate (17 * 8) 'w'

-- | 1152 bits = 144 bytes = 18 beats (exactly two SHA3-512 blocks)
msg1152 :: ByteString
msg1152 = BS8.replicate (18 * 8) 'x'

msg1344 :: ByteString
msg1344 = BS8.replicate (21 * 8) 'a'

msg1408 :: ByteString
msg1408 = BS8.replicate (22 * 8) 'b'

msg1600 :: ByteString
msg1600 = BS8.pack $ take 200 (cycle "0123456789")

msg2016 :: ByteString
msg2016 = BS8.pack $ take 256 (cycle "0123456789abcdef")  -- 256 bytes = 32 * 8 (multiple of 8)

msg2176 :: ByteString
msg2176 = BS8.pack $ take 272 (cycle "abcdef0123456789")

msg2688 :: ByteString
msg2688 = BS8.pack $ take 336 (cycle "qwertyuiopasdfgh")

msg3200 :: ByteString
msg3200 = BS8.pack $ take 400 (cycle "abcdef0123456789")

msg3264 :: ByteString
msg3264 = BS8.pack $ take 408 (cycle "0123456789abcdef")

msg8 :: ByteString
msg8 = "qwertyui"

--------------------------------------------------------------------------------
-- Shared stall / backpressure patterns
--------------------------------------------------------------------------------

stallPatternSimple, stallPatternModerate, stallPatternAggressive :: [Bool]
stallPatternSimple =
  [True, True, False, True, True, True, False, True, True, True]
stallPatternModerate =
  [True, False, True, False, True, False, True, False, True, False]
stallPatternAggressive =
  [True, False, False, True, False, True, False, False, False, True]

backpressurePatternSimple, backpressurePatternModerate, backpressurePatternAggressive :: [Bool]
backpressurePatternSimple =
  [True, True, True, False, True, True, True, False, True, True]
backpressurePatternModerate =
  [True, False, True, False, True, False, True, False, True, False]
backpressurePatternAggressive =
  [True, False, False, False, True, False, False, True, False, False]

shake128Gen :: Gen ShakeTest
shake128Gen = genShakeTest shake128GenConfig

shake256Gen :: Gen ShakeTest
shake256Gen = genShakeTest shake256GenConfig

--------------------------------------------------------------------------------
-- Shared ShakeTest suites
--------------------------------------------------------------------------------

basicCases :: [ShakeTest]
basicCases =
  [ makeBasicTest BS8.empty 32,
    makeBasicTest "qwertyui" 32,
    makeBasicTest msg1088 32,
    makeBasicTest msg1600 32,
    makeBasicTest msg3200 32,
    makeBasicTest msg1344 32,
    makeBasicTest msg2016 32,
    makeBasicTest msg2688 64
  ]

variableOutputCases :: [ShakeTest]
variableOutputCases =
  [ makeVariableOutputTest "minimal8" 8,
    makeVariableOutputTest "16byte_test_data" 40,
    makeVariableOutputTest "testdata" 256
  ]

stallCases :: [ShakeTest]
stallCases =
  [ makeStallTest msg1344 64 stallPatternAggressive,
    makeStallTest msg2016 32 stallPatternModerate,
    makeStallTest msg1088 32 stallPatternAggressive
  ]

backpressureCases :: [ShakeTest]
backpressureCases =
  [ makeBackpressureTest msg1344 128 backpressurePatternSimple,
    makeCombinedTest
      msg1408
      256
      stallPatternSimple
      backpressurePatternAggressive,
    makeCombinedTest
      msg1088
      256
      stallPatternAggressive
      backpressurePatternAggressive
  ]

--------------------------------------------------------------------------------
-- SHA3-256 deterministic cases
--------------------------------------------------------------------------------

sha3BasicCases :: [ShakeTest]
sha3BasicCases =
  [ makeBasicTest msg8 32,
    makeBasicTest msg1088 32,
    makeBasicTest msg1600 32,
    makeBasicTest msg3200 32
  ]

sha3StallCases :: [ShakeTest]
sha3StallCases =
  [ makeStallTest msg1088 32 stallPatternAggressive
  ]

sha3BackpressureCases :: [ShakeTest]
sha3BackpressureCases =
  [ makeCombinedTest
      msg1088
      32
      stallPatternAggressive
      backpressurePatternAggressive
  ]

--------------------------------------------------------------------------------
-- SHA3-512 deterministic cases
--------------------------------------------------------------------------------

sha3512BasicCases :: [ShakeTest]
sha3512BasicCases =
  [ makeBasicTest BS8.empty 64,       -- empty input
    makeBasicTest msg8 64,            -- minimal input (8 bytes)
    makeBasicTest msg576 64,          -- exactly one block (72 bytes)
    makeBasicTest msg1152 64,         -- exactly two blocks (144 bytes)
    makeBasicTest msg1600 64,         -- multi-block
    makeBasicTest msg3200 64          -- large multi-block
  ]

sha3512StallCases :: [ShakeTest]
sha3512StallCases =
  [ makeStallTest msg576 64 stallPatternAggressive,
    makeStallTest msg1152 64 stallPatternModerate,
    makeStallTest msg1600 64 stallPatternSimple
  ]

sha3512BackpressureCases :: [ShakeTest]
sha3512BackpressureCases =
  [ makeBackpressureTest msg576 64 backpressurePatternSimple,
    makeBackpressureTest msg1152 64 backpressurePatternModerate,
    makeCombinedTest
      msg576
      64
      stallPatternAggressive
      backpressurePatternAggressive,
    makeCombinedTest
      msg1600
      64
      stallPatternModerate
      backpressurePatternSimple
  ]
