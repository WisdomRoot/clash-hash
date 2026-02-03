{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TestHarness.StreamCommon
  ( UpstreamStall (..),
    DownstreamBackpressure (..),
    feedInput,
    feedInput256,
    makeBackpressureSignal,
    bsToBitListHW,
    bitListToBSHW,
    bitListToWords,
    wordToBits,
  )
where

import AXI4Stream (AXI4Stream (..))
import Clash.Prelude hiding (tlast)
import Clash.Sized.Vector qualified as V
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.QuickCheck (Arbitrary (..), frequency, listOf1)
import Prelude qualified as P

--------------------------------------------------------------------------------
-- Shared data types
--------------------------------------------------------------------------------

data UpstreamStall
  = NoUpstreamStall
  | UpstreamStall [Bool]
  deriving (Show)

data DownstreamBackpressure
  = NoDownstreamBackpressure
  | DownstreamBackpressure [Bool]
  deriving (Show)

instance Arbitrary UpstreamStall where
  arbitrary =
    frequency
      [ (3, pure NoUpstreamStall),
        (1, UpstreamStall <$> listOf1 (frequency [(3, pure True), (1, pure False)]))
      ]

instance Arbitrary DownstreamBackpressure where
  arbitrary =
    frequency
      [ (3, pure NoDownstreamBackpressure),
        (1, DownstreamBackpressure <$> listOf1 (frequency [(3, pure True), (1, pure False)]))
      ]

--------------------------------------------------------------------------------
-- Input / backpressure helpers
--------------------------------------------------------------------------------

feedInput ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  Int ->
  UpstreamStall ->
  Vec beats (BitVector 64) ->
  Signal dom (AXI4Stream 64, Bool)
feedInput beatsPerBlock control messageWords =
  let isEmpty = V.length messageWords == 0
   in mealy step (toList messageWords, 0 :: Int, 0 :: Int, stallPattern, isEmpty) (pure ())
  where
    stallPattern = case control of
      NoUpstreamStall -> []
      UpstreamStall xs -> xs
    permuteLatency = 24 :: Int
    step (xs, waitCount, emittedInBlock, ctrl, wasEmpty) _ =
      if waitCount P.> 0
        then ((xs, waitCount P.- 1, emittedInBlock, ctrl, wasEmpty), (idleBeat, False))
        else
          let (canSend, ctrl') = case ctrl of
                [] -> (True, [])
                b : bs -> (b, bs)
           in if P.not canSend
                then ((xs, waitCount, emittedInBlock, ctrl', wasEmpty), (idleBeat, False))
                else case xs of
                  []
                    | wasEmpty ->
                        (([], 0, 0, ctrl', False), (idleBeat, True))
                  [] -> (([], 0, 0, ctrl', False), (idleBeat, False))
                  y : ys ->
                    let isLast = P.null ys
                        emittedNow = emittedInBlock P.+ 1
                        blockCompleted = emittedNow == beatsPerBlock
                        needGap = blockCompleted P.&& P.not isLast
                        nextWait = if needGap then permuteLatency else 0
                        nextEmitted = if blockCompleted then 0 else emittedNow
                        nextState = (ys, nextWait, nextEmitted, ctrl', False)
                        outBeat =
                          AXI4Stream
                            { tdata = y,
                              tvalid = True,
                              tlast = isLast
                            }
                     in (nextState, (outBeat, False))
    idleBeat =
      AXI4Stream
        { tdata = 0,
          tvalid = False,
          tlast = False
        }

feedInput256 ::
  forall beats dom.
  ( KnownNat beats,
    HiddenClockResetEnable dom
  ) =>
  Int ->
  UpstreamStall ->
  Vec beats (BitVector 256) ->
  Signal dom (AXI4Stream 256, Bool)
feedInput256 beatsPerBlock control messageWords =
  let isEmpty = V.length messageWords == 0
   in mealy step (toList messageWords, 0 :: Int, 0 :: Int, stallPattern, isEmpty) (pure ())
  where
    stallPattern = case control of
      NoUpstreamStall -> []
      UpstreamStall xs -> xs
    permuteLatency = 24 :: Int
    step (xs, waitCount, emittedInBlock, ctrl, wasEmpty) _ =
      if waitCount P.> 0
        then ((xs, waitCount P.- 1, emittedInBlock, ctrl, wasEmpty), (idleBeat, False))
        else
          let (canSend, ctrl') = case ctrl of
                [] -> (True, [])
                b : bs -> (b, bs)
           in if P.not canSend
                then ((xs, waitCount, emittedInBlock, ctrl', wasEmpty), (idleBeat, False))
                else case xs of
                  []
                    | wasEmpty ->
                        (([], 0, 0, ctrl', False), (idleBeat, True))
                  [] -> (([], 0, 0, ctrl', False), (idleBeat, False))
                  y : ys ->
                    let isLast = P.null ys
                        emittedNow = emittedInBlock P.+ 1
                        blockCompleted = emittedNow == beatsPerBlock
                        needGap = blockCompleted P.&& P.not isLast
                        nextWait = if needGap then permuteLatency else 0
                        nextEmitted = if blockCompleted then 0 else emittedNow
                        nextState = (ys, nextWait, nextEmitted, ctrl', False)
                        outBeat =
                          AXI4Stream
                            { tdata = y,
                              tvalid = True,
                              tlast = isLast
                            }
                     in (nextState, (outBeat, False))
    idleBeat =
      AXI4Stream
        { tdata = 0,
          tvalid = False,
          tlast = False
        }

makeBackpressureSignal :: DownstreamBackpressure -> Signal System Bool
makeBackpressureSignal NoDownstreamBackpressure = pure True
makeBackpressureSignal (DownstreamBackpressure pattern) =
  fromList (pattern P.++ P.repeat True)

--------------------------------------------------------------------------------
-- Conversion helpers
--------------------------------------------------------------------------------

bsToBitListHW :: ByteString -> [Bit]
bsToBitListHW bs =
  P.concatMap word8ToBits (BS.unpack bs)
  where
    word8ToBits :: Word8 -> [Bit]
    word8ToBits w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 7]]

bitListToBSHW :: [Bit] -> ByteString
bitListToBSHW bits =
  BS.pack (packBytes bits)
  where
    packBytes [] = []
    packBytes bs =
      let (chunk, rest) = P.splitAt 8 bs
          byte = P.foldl setBit' 0 (P.zip [0 ..] chunk)
       in byte : packBytes rest
    setBit' acc (i, b) = if b == 1 then Bits.setBit acc i else acc

bitListToWords :: forall beats. (KnownNat beats) => Int -> [Bit] -> Vec beats (BitVector 64)
bitListToWords n bits =
  let chunks = chunksOf 64 bits
      wordsList = P.map bitsToWord (P.take n chunks)
   in V.unsafeFromList wordsList
  where
    chunksOf _ [] = []
    chunksOf m xs = P.take m xs : chunksOf m (P.drop m xs)
    bitsToWord bs =
      let paddedBits = P.take 64 (bs P.++ P.repeat 0)
          word = P.foldl accumBit 0 (P.zip [63, 62 .. 0] paddedBits)
       in word
    accumBit acc (i, b) = if b == 1 then Bits.setBit acc i else acc

wordToBits :: BitVector 64 -> [Bit]
wordToBits w = [if Bits.testBit w i then 1 else 0 | i <- [63, 62 .. 0]]
