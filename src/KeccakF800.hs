{-# LANGUAGE TypeApplications #-}

module KeccakF800
  ( -- * Round primitives
    thetaF800,
    rhoF800,
    piF800,
    chiF800,
    iotaF800,
    -- * Permutation
    keccakF800Round,
    keccakF800,
    keccakF800Sponge,
    -- * SHA3-f[800] specific
    padSHA3,
    -- * Top entities
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants
import Sponge (SpongeParameter, sponge)

--------------------------------------------------------------------------------
-- Round primitives
--------------------------------------------------------------------------------

thetaF800 :: BitVector 800 -> BitVector 800
thetaF800 bv =
  ifoldl
    ( \acc idx indices11 ->
        let bitOut = fold xor (map (bv !) indices11)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.theta 5)

chiF800 :: BitVector 800 -> BitVector 800
chiF800 bv =
  ifoldl
    ( \acc idx (i0, i1, i2) ->
        let bitOut = bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.chi 5)

piF800 :: BitVector 800 -> BitVector 800
piF800 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.pi 5)

rhoF800 :: BitVector 800 -> BitVector 800
rhoF800 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.rho 5)

iotaF800 :: Index 24 -> BitVector 800 -> BitVector 800
iotaF800 roundIdx bv =
  let lane0 = slice d31 d0 bv
      lane0' = lane0 `xor` truncateB ($(Constants.iota) !! roundIdx)
   in slice d799 d32 bv ++# lane0'

--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

keccakF800Round :: Index 24 -> BitVector 800 -> BitVector 800
keccakF800Round roundIdx =
  iotaF800 roundIdx . chiF800 . piF800 . rhoF800 . thetaF800

keccakF800 :: BitVector 800 -> BitVector 800
keccakF800 initialState =
  foldl applyRound initialState (indicesI @22)
  where
    applyRound state roundIdx = keccakF800Round (resize roundIdx) state

keccakF800Sponge :: forall r n m k d. (SpongeParameter 800 r n m k d) => BitVector m -> BitVector d
keccakF800Sponge = sponge @800 @r @n @m @k @d keccakF800

--------------------------------------------------------------------------------
-- SHA3-f[800] parameters
--------------------------------------------------------------------------------

type Rate = 384

type Capacity = 416

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
  deriving (Generic, NFDataX, Eq, Show)

data SHA3State maxBlocks = SHA3State
  { sha3StateData :: BitVector 800,
    sha3RoundCounter :: Index 22,
    sha3Phase :: Phase,
    sha3Active :: Bool,
    sha3BlocksRemaining :: Unsigned 8,
    sha3SqueezedBits :: Unsigned 16,
    sha3LatchedBlocks :: Vec maxBlocks (BitVector Rate)
  }
  deriving (Generic, NFDataX)

sha3f800Seq ::
  forall dom maxBlocks.
  ( HiddenClockResetEnable dom,
    KnownNat maxBlocks,
    1 <= maxBlocks
  ) =>
  Signal dom Bool ->
  Signal dom (Vec maxBlocks (BitVector Rate)) ->
  Signal dom (Bool, BitVector DigestBits)
sha3f800Seq start blocks = mealy step initialState (bundle (start, blocks))
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

          stateData' =
            if sha3Active stateAfterAbsorb
              then keccakF800Round (resize (sha3RoundCounter stateAfterAbsorb)) (sha3StateData stateAfterAbsorb)
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
-- Top entity (demo)
--------------------------------------------------------------------------------

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF800_SHA3",
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

type MessageBits = 384

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (BitVector MessageBits) ->
  Signal System (Bool, BitVector DigestBits)
topEntity clk rst en msgStart msgData =
  withClockResetEnable clk rst en $
    sha3f800Seq @System @MaxBlocks msgStart paddedBlocks
  where
    paddedBlocks = fmap (padSHA3 @Rate @MessageBits @MaxBlocks) msgData
