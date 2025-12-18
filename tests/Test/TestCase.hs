{-# LANGUAGE GADTs #-}

module Test.TestCase
  ( TestCase (..),
    SomeMessage (..),
    Control (..),
    run,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Hash.NonPipelined qualified
import Test.Hspec
import Prelude qualified as P

data SomeMessage where
  SomeMessage :: (KnownNat beats) => Vec (beats * 64) Bit -> SomeMessage

data TestCase = TestCase
  { testCaseLabel :: String,
    testCaseMessage :: SomeMessage,
    testCaseExpected :: Vec 4 (BitVector 64),
    testCaseControl :: Control
  }

data Control
  = NoUpstreamStall
  | UpstreamStall [Bool]

run ::
  TestCase ->
  Expectation
run (TestCase _ (SomeMessage (message :: Vec (beats * 64) Bit)) expected control) = do
  let messageWords = bitCoerce message :: Vec beats (BitVector 64)
      inputStream =
        withClockResetEnable clockGen resetGen enableGen
          $ feedInput control messageWords
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

feedInput ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  Control ->
  Vec beats (BitVector 64) ->
  Signal dom (AXI4Stream 64)
feedInput control messageWords =
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
