{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.NonPipelined (spec) where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Data.Foldable (for_)
import Hash.NonPipelined qualified
import Reference.SHA3 qualified as SHA3
import Reference.SHA3internal qualified as SHA3internal
import Test.Hspec
import Prelude qualified as P

spec :: Spec
spec = describe "NonPipelined SHA3-256 Tests" $ do
  for_ testCases $ \(TestCase label message expected control) ->
    it label $ runCase message expected control

--

data SomeMessage where
  SomeMessage :: KnownNat beats => Vec (beats * 64) Bit -> SomeMessage

data TestCase = TestCase
  { npLabel :: String,
    npMessage :: SomeMessage,
    npExpected :: Vec 4 (BitVector 64),
    npInputControl :: NPInputControl
  }

data NPInputControl
  = NoUpstreamStall
  | UpstreamStall [Bool]

testCases :: [TestCase]
testCases =
  [ TestCase "64-bit input" (SomeMessage msg64) expected64 NoUpstreamStall,
    TestCase "128-bit input" (SomeMessage msg128) expected128 NoUpstreamStall,
    TestCase "1024-bit input" (SomeMessage msg1024) expected1024 NoUpstreamStall,
    TestCase "1088-bit input" (SomeMessage msg1088) expected1088 NoUpstreamStall,
    TestCase "1600-bit input" (SomeMessage msg1600) expected1600 NoUpstreamStall,
    TestCase "128-bit input (upstream stalls)" (SomeMessage msg128) expected128 (UpstreamStall stallPattern)
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

runCase ::
  SomeMessage ->
  Vec 4 (BitVector 64) ->
  NPInputControl ->
  Expectation
runCase (SomeMessage (message :: Vec (beats * 64) Bit)) expected control = do
  let messageWords = bitCoerce message :: Vec beats (BitVector 64)
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ runInputStream control messageWords
      output =
        Hash.NonPipelined.topEntity
          clockGen
          resetGen
          enableGen
          (pure True)
          inputStream
      beatCount = natToNum @beats :: Int
      blockTransitions =
        if beatCount <= 0
          then 0
          else P.max 0 (beatCount - 1) `div` 17
      gapCycles = blockTransitions * 24
      permutationCount = blockTransitions + 1
      sampleCount = beatCount + gapCycles + 1 + permutationCount * 24 + 4 + 64
      samples = sampleN @System sampleCount output
      actualStreams = P.take 4 $ P.filter (tvalid . fst) samples

  fmap (tdata . fst) actualStreams `shouldBe` toList expected
  fmap (tvalid . fst) actualStreams `shouldBe` P.replicate 4 True
  fmap (tlast . fst) actualStreams `shouldBe` [False, False, False, True]

runInputStream ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  NPInputControl ->
  Vec beats (BitVector 64) ->
  Signal dom (AXI4Stream 64)
runInputStream control messageWords =
  mealy step (toList messageWords, 0 :: Int, 0 :: Int, controlToList control) (pure ())
  where
    controlToList NoUpstreamStall = []
    controlToList (UpstreamStall xs) = xs

    step (xs, waitCount, emittedInBlock, ctrl) _ =
      let (canSend, ctrl') =
            case ctrl of
              [] -> (True, [])
              b : bs -> (b, bs)
       in if waitCount > 0
            then ((xs, waitCount - 1, emittedInBlock, ctrl'), idleBeat)
            else
              if not canSend
                then ((xs, waitCount, emittedInBlock, ctrl'), idleBeat)
                else case xs of
                  [] -> (([], 0, 0, ctrl'), idleBeat)
                  y : ys ->
                    let isLast = P.null ys
                        emittedNow = emittedInBlock + 1
                        needGap = emittedNow == 17 && not isLast
                        nextWait = if needGap then 24 else 0
                        nextEmitted = if needGap then 0 else emittedNow
                        nextState = (ys, nextWait, nextEmitted, ctrl')
                        outBeat =
                          AXI4Stream
                            { tdata = y,
                              tvalid = True,
                              tlast = isLast
                            }
                     in (nextState, outBeat)
    idleBeat =
      AXI4Stream
        { tdata = 0,
          tvalid = False,
          tlast = False
        }
