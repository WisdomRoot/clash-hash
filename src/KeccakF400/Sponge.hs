{-# LANGUAGE TypeApplications #-}

module KeccakF400.Sponge
  ( -- * SHA3-f[400] parameters
    Rate,
    Capacity,
    DigestBits,
    SHA3SuffixBits,
    sha3Suffix,
    -- * Padding
    padSHA3,
    -- * FSM types
    Phase (..),
    SHA3State (..),
    -- * Top entity
    topEntity,
  )
where

import Clash.Prelude
import qualified KeccakF400.Permutation as Perm

--------------------------------------------------------------------------------
-- SHA3-f[400] parameters
--------------------------------------------------------------------------------

type Rate = 256

type Capacity = 144

type DigestBits = 256

type SHA3SuffixBits = 2

sha3Suffix :: BitVector SHA3SuffixBits
sha3Suffix = 0b01

--------------------------------------------------------------------------------
-- Padding
--------------------------------------------------------------------------------

padSHA3 ::
  forall rate msgLen numBlocks.
  ( KnownNat rate,
    KnownNat msgLen,
    KnownNat numBlocks,
    KnownNat (numBlocks * rate),
    KnownNat (msgLen + SHA3SuffixBits),
    KnownNat (msgLen + SHA3SuffixBits + 1),
    KnownNat (numBlocks * rate - (msgLen + SHA3SuffixBits + 2)),
    (msgLen + SHA3SuffixBits + 2) <= numBlocks * rate
  ) =>
  BitVector msgLen ->
  Vec numBlocks (BitVector rate)
padSHA3 msg =
  let buildPadded :: BitVector msgLen -> BitVector (numBlocks * rate)
      buildPadded m =
        m
          ++# sha3Suffix
          ++# (1 :: BitVector 1)
          ++# (0 :: BitVector (numBlocks * rate - (msgLen + SHA3SuffixBits + 2)))
          ++# (1 :: BitVector 1)

      paddedBits = leToPlusKN @(msgLen + SHA3SuffixBits + 2) @(numBlocks * rate) buildPadded msg
   in unconcatBitVector# @numBlocks @rate paddedBits

--------------------------------------------------------------------------------
-- Sequential multi-block FSM
--------------------------------------------------------------------------------

data Phase = Absorbing | Squeezing | Idle
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)

data SHA3State maxBlocks = SHA3State
  { sha3StateData :: BitVector 400,
    sha3RoundCounter :: Index 20,
    sha3Phase :: Phase,
    sha3Active :: Bool,
    sha3BlocksRemaining :: Unsigned 8,
    sha3SqueezedBits :: Unsigned 16,
    sha3LatchedBlocks :: Vec maxBlocks (BitVector Rate)
  }
  deriving stock (Generic)
  deriving anyclass (NFDataX)

sha3f400Seq ::
  forall dom maxBlocks.
  ( HiddenClockResetEnable dom,
    KnownNat maxBlocks,
    1 <= maxBlocks
  ) =>
  Signal dom Bool ->
  Signal dom (Vec maxBlocks (BitVector Rate)) ->
  Signal dom (Bool, BitVector DigestBits)
sha3f400Seq start blocks = mealy step initialState (bundle (start, blocks))
  where
    initialState =
      SHA3State
        { sha3StateData = 0,
          sha3RoundCounter = 0,
          sha3Phase = Idle,
          sha3Active = False,
          sha3BlocksRemaining = 0,
          sha3SqueezedBits = 0,
          sha3LatchedBlocks = repeat 0
        }

    step st (startPulse, blockVec) =
      let currentPhase = sha3Phase st
          currentRound = sha3RoundCounter st

          stateAfterLoad
            | currentPhase == Idle && startPulse =
                let totalBlocks = natToNum @maxBlocks :: Int
                 in st
                      { sha3StateData = 0,
                        sha3RoundCounter = 0,
                        sha3Active = False,
                        sha3Phase = Absorbing,
                        sha3BlocksRemaining = fromIntegral (totalBlocks - 1),
                        sha3SqueezedBits = 0,
                        sha3LatchedBlocks = blockVec
                      }
            | otherwise = st

          stateAfterAbsorb
            | sha3Phase stateAfterLoad == Absorbing && not (sha3Active stateAfterLoad) =
                let totalBlocks = natToNum @maxBlocks :: Int
                    blocksRemaining = fromIntegral (sha3BlocksRemaining stateAfterLoad) :: Int
                    blockIdx = totalBlocks - blocksRemaining - 1
                    block = sha3LatchedBlocks stateAfterLoad !! blockIdx
                 in stateAfterLoad
                      { sha3StateData = sha3StateData stateAfterLoad `xor` ((0 :: BitVector Capacity) ++# block),
                        sha3Active = True,
                        sha3RoundCounter = 0
                      }
            | otherwise = stateAfterLoad

          -- Execute round if active using the topEntity from Permutation module
          -- Clash will instantiate KeccakF400_Round instead of inlining
          stateData' =
            if sha3Active stateAfterAbsorb
              then Perm.topEntity (resize (sha3RoundCounter stateAfterAbsorb), sha3StateData stateAfterAbsorb)
              else sha3StateData stateAfterAbsorb

          nextRound
            | sha3Active stateAfterAbsorb && currentRound == maxBound = 0
            | sha3Active stateAfterAbsorb = currentRound + 1
            | otherwise = currentRound

          permutationComplete = sha3Active stateAfterAbsorb && currentRound == maxBound

          blocksLeft = sha3BlocksRemaining stateAfterAbsorb
          squeezedBits = sha3SqueezedBits stateAfterAbsorb

          digestFitsInOneBlock = (natToNum @DigestBits :: Int) <= (natToNum @Rate :: Int)

          (nextPhase, nextActive, nextBlocksLeft, nextSqueezed) =
            case (sha3Phase stateAfterAbsorb, permutationComplete) of
              (Absorbing, True) | blocksLeft == 0 ->
                if digestFitsInOneBlock
                  then (Idle, False, 0, fromIntegral (natToNum @Rate :: Int))
                  else (Squeezing, True, 0, fromIntegral (natToNum @Rate :: Int))
              (Absorbing, True) ->
                (Absorbing, False, blocksLeft - 1, 0)
              (Squeezing, True) ->
                let newSqueezed = squeezedBits + fromIntegral (natToNum @Rate :: Int)
                 in if newSqueezed >= fromIntegral (natToNum @DigestBits :: Int)
                      then (Idle, False, 0, 0)
                      else (Squeezing, True, 0, newSqueezed)
              (_, False) ->
                (sha3Phase stateAfterAbsorb, sha3Active stateAfterAbsorb, blocksLeft, squeezedBits)
              _ ->
                (sha3Phase stateAfterAbsorb, sha3Active stateAfterAbsorb, blocksLeft, squeezedBits)

          digestReady =
            permutationComplete
              && ( sha3Phase stateAfterAbsorb == Absorbing && blocksLeft == 0
                   || sha3Phase stateAfterAbsorb == Squeezing
                 )
              && nextPhase == Idle

          digestOut = leToPlusKN @DigestBits @Rate truncateB (truncateB @_ @Rate stateData')

          nextState =
            SHA3State
              { sha3StateData = stateData',
                sha3RoundCounter = nextRound,
                sha3Phase = nextPhase,
                sha3Active = nextActive,
                sha3BlocksRemaining = nextBlocksLeft,
                sha3SqueezedBits = nextSqueezed,
                sha3LatchedBlocks = sha3LatchedBlocks stateAfterAbsorb
              }
       in (nextState, (digestReady, digestOut))

--------------------------------------------------------------------------------
-- Top entity
--------------------------------------------------------------------------------

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF400_SHA3",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "MSG_START",
            PortName "MSG_DATA"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "DIGEST_VALID",
              PortName "DIGEST_DATA"
            ]
      }
  )
  #-}
{-# OPAQUE topEntity #-}

type MaxBlocks = 2

type MessageBits = 256

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (BitVector MessageBits) ->
  Signal System (Bool, BitVector DigestBits)
topEntity clk rst en msgStart msgData =
  withClockResetEnable clk rst en $
    sha3f400Seq @System @MaxBlocks msgStart paddedBlocks
  where
    paddedBlocks = fmap (padSHA3 @Rate @MessageBits @MaxBlocks) msgData
