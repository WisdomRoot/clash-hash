{-# LANGUAGE TypeApplications #-}

module KeccakF1600
  ( -- * Round primitives
    thetaF1600,
    rhoF1600,
    piF1600,
    chiF1600,
    iotaF1600,
    -- * Permutation
    keccakF1600Round,
    keccakF1600,
    keccakF1600Sponge,
    -- * SHA3-f[1600] specific
    padSHA3,
    -- * Top entities
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants
import Sponge (SpongeParameter, sponge)
import SHA3internal (_iota_constants)

--------------------------------------------------------------------------------
-- Round primitives
--------------------------------------------------------------------------------

thetaF1600 :: BitVector 1600 -> BitVector 1600
thetaF1600 bv =
  ifoldl
    ( \acc idx indices11 ->
        let bitOut = fold xor (map (bv !) indices11)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.theta 6)

chiF1600 :: BitVector 1600 -> BitVector 1600
chiF1600 bv =
  ifoldl
    ( \acc idx (i0, i1, i2) ->
        let bitOut = bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.chi 6)

piF1600 :: BitVector 1600 -> BitVector 1600
piF1600 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.pi 6)

rhoF1600 :: BitVector 1600 -> BitVector 1600
rhoF1600 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.rho 6)

-- Precomputed 64-bit round constants
{-# NOINLINE iotaConstants64 #-}
iotaConstants64 :: Vec 24 (BitVector 64)
iotaConstants64 = map bitCoerce _iota_constants

iotaF1600 :: Index 24 -> BitVector 1600 -> BitVector 1600
iotaF1600 roundIdx bv =
  let (rest :: BitVector (1600 - 64), lane0 :: BitVector 64) = split bv
      rc = iotaConstants64 !! roundIdx
      lane0' = lane0 `xor` rc
   in rest ++# lane0'

--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

keccakF1600Round :: Index 24 -> BitVector 1600 -> BitVector 1600
keccakF1600Round roundIdx =
  iotaF1600 roundIdx . chiF1600 . piF1600 . rhoF1600 . thetaF1600

keccakF1600 :: BitVector 1600 -> BitVector 1600
keccakF1600 initialState =
  foldl applyRound initialState (indicesI @24)
  where
    applyRound state roundIdx = keccakF1600Round roundIdx state

keccakF1600Sponge :: forall r n m k d. (SpongeParameter 1600 r n m k d) => BitVector m -> BitVector d
keccakF1600Sponge = sponge @1600 @r @n @m @k @d keccakF1600

--------------------------------------------------------------------------------
-- SHA3-f[1600] parameters
--------------------------------------------------------------------------------

type Rate = 1088

type Capacity = 512

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
-- Sequential sponge state
--------------------------------------------------------------------------------

data Phase = Absorbing | Squeezing | Idle
  deriving (Generic, NFDataX, Eq, Show)

data SpongeState = SpongeState
  { stateData :: BitVector 1600,
    roundCounter :: Index 24,
    active :: Bool
  }
  deriving (Generic, NFDataX)

data SHA3State maxBlocks = SHA3State
  { sha3StateData :: BitVector 1600,
    sha3RoundCounter :: Index 24,
    sha3Phase :: Phase,
    sha3Active :: Bool,
    sha3BlocksRemaining :: Unsigned 16,
    sha3SqueezedBits :: Unsigned 16,
    sha3LatchedBlocks :: Vec maxBlocks (BitVector Rate)
  }
  deriving (Generic, NFDataX)

sha3f1600Seq ::
  forall dom maxBlocks.
  ( HiddenClockResetEnable dom,
    KnownNat maxBlocks,
    1 <= maxBlocks
  ) =>
  Signal dom Bool ->
  Signal dom (Vec maxBlocks (BitVector Rate)) ->
  Signal dom (Bool, BitVector DigestBits)
sha3f1600Seq start blocks = mealy step initialState (bundle (start, blocks))
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
              then keccakF1600Round (sha3RoundCounter stateAfterAbsorb) (sha3StateData stateAfterAbsorb)
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
      { t_name = "KeccakF1600_SHA3",
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

type MessageBits = 2048

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (BitVector MessageBits) ->
  Signal System (Bool, BitVector DigestBits)
topEntity clk rst en msgStart msgData =
  withClockResetEnable clk rst en $
    sha3f1600Seq @System @MaxBlocks msgStart paddedBlocks
  where
    paddedBlocks = fmap (padSHA3 @Rate @MessageBits @MaxBlocks) msgData
