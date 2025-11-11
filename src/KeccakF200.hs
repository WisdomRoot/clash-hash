{-# LANGUAGE TypeApplications #-}

module KeccakF200
  ( -- * Round primitives
    thetaF200,
    rhoF200,
    piF200,
    chiF200,
    iotaF200,
    -- * Permutation
    keccakF200Round,
    keccakF200,
    keccakF200Sponge,
    -- * SHA3-f[200] specific
    padSHA3,
    -- * Top entities
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants
import Sponge (SpongeParameter, sponge)

-- Theta transformation: XOR with column parities
thetaF200 :: BitVector 200 -> BitVector 200
thetaF200 bv =
  ifoldl
    ( \acc idx indices11 ->
        let bitOut = fold xor (map (bv !) indices11)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.theta 3)

-- Chi transformation expressed directly on BitVector
chiF200 :: BitVector 200 -> BitVector 200
chiF200 bv =
  ifoldl
    ( \acc idx (i0, i1, i2) ->
        let bitOut = bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.chi 3)

-- Pi transformation: bit permutation on BitVector
piF200 :: BitVector 200 -> BitVector 200
piF200 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.pi 3)

-- Rho transformation: bit permutation on BitVector (lane rotation)
rhoF200 :: BitVector 200 -> BitVector 200
rhoF200 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.rho 3)

iotaF200 :: Index 24 -> BitVector 200 -> BitVector 200
iotaF200 roundIdx bv =
  let lane0 = slice d7 d0 bv -- Extract first 8 bits (lane 0)
      lane0' = lane0 `xor` truncateB ($(Constants.iota) !! roundIdx) -- XOR with selected round constant
   in slice d199 d8 bv ++# lane0' -- Replace bits 0-7 with result

-- Complete Keccak-f[200] round: Theta, Rho, Pi, Chi, Iota
keccakF200Round :: Index 24 -> BitVector 200 -> BitVector 200
keccakF200Round roundIdx =
  iotaF200 roundIdx . chiF200 . piF200 . rhoF200 . thetaF200

-- | Full Keccak-f[200] permutation: 18 rounds (12 + 2*l where l=3)
-- Applies all rounds in sequence using the round constants
keccakF200 :: BitVector 200 -> BitVector 200
keccakF200 initialState =
  foldl applyRound initialState (indicesI @18)
  where
    applyRound state roundIdx = keccakF200Round (resize roundIdx) state

-- | Keccak-f[200] sponge function with configurable rate/capacity.
-- Instantiation: r=144, capacity=56, message=140 bits, output=128 bits
-- n = (140 + 144 + 1) / 144 = 1 absorb block
-- k = 128 / 144 = 0 squeeze blocks (128 < 144, so 1 total block suffices)
keccakF200Sponge :: forall r n m k d. (SpongeParameter 200 r n m k d) => BitVector m -> BitVector d
keccakF200Sponge = sponge @200 @r @n @m @k @d keccakF200

--------------------------------------------------------------------------------
-- SHA3-f[200] Parameters
--------------------------------------------------------------------------------

-- | Concrete parameters for SHA3-f[200] (pedagogical 200-bit variant)
type Rate = 144          -- Rate: bits XORed per absorb/squeeze
type Capacity = 56       -- Capacity: 200 - Rate = 56
type DigestBits = 128    -- Output digest length
type SHA3SuffixBits = 2  -- Domain separation suffix length

-- | SHA3 domain separation suffix: 0b01
sha3Suffix :: BitVector SHA3SuffixBits
sha3Suffix = 0b01

--------------------------------------------------------------------------------
-- SHA3 Padding (pad10*1 + domain suffix)
--------------------------------------------------------------------------------

-- | Pad message with SHA3 suffix and pad10*1 rule to fill rate blocks.
--
-- = Algorithm
--
-- 1. Append SHA3 suffix bits (0b01) after message
-- 2. Append '1' bit (start of pad10*1)
-- 3. Append zeros to fill to rate boundary
-- 4. Set final bit to '1' (end of pad10*1)
--
-- = Example (rate=144, msg=140 bits)
--
-- @
-- Message:     MMM...MMM (140 bits)
-- + Suffix:    MMM...MMM 01 (142 bits)
-- + Pad start: MMM...MMM 01 1 (143 bits)
-- + Pad end:   MMM...MMM 01 1 1 (144 bits = 1 block, no zeros needed)
-- @
--
-- The padding ensures total length is multiple of rate.
-- Caller must provide correct numBlocks via type application.
padSHA3 ::
  forall rate msgLen numBlocks.
  ( KnownNat rate,
    KnownNat msgLen,
    KnownNat numBlocks,
    KnownNat (numBlocks * rate),
    KnownNat (msgLen + SHA3SuffixBits),
    KnownNat (msgLen + SHA3SuffixBits + 1),
    KnownNat (msgLen + SHA3SuffixBits + 1 + (numBlocks * rate - (msgLen + SHA3SuffixBits + 2))),
    (msgLen + SHA3SuffixBits + 2) <= numBlocks * rate
  ) =>
  BitVector msgLen ->
  Vec numBlocks (BitVector rate)
padSHA3 msg =
  let -- Append suffix after message
      msgWithSuffix = msg ++# sha3Suffix :: BitVector (msgLen + SHA3SuffixBits)

      -- Build: msg ++ suffix ++ '1' ++ zeros ++ '1'
      -- Use ++# to build from LSB to MSB (same as Sponge.hs padding)
      -- unconcatBitVector# splits MSB-first, so high bits become first block
      msgWithPad1 = msgWithSuffix ++# (1 :: BitVector 1)
                    :: BitVector (msgLen + SHA3SuffixBits + 1)

      -- Calculate zeros needed at runtime, then extend
      totalBits = natToNum @(numBlocks * rate) :: Int

      -- Use resize and replaceBit to avoid complex type-level arithmetic
      extended = resize msgWithPad1 :: BitVector (numBlocks * rate)
      paddedBits = replaceBit (totalBits - 1) high extended

   in unconcatBitVector# @numBlocks @rate paddedBits

--------------------------------------------------------------------------------
-- Sequential Multi-Block Sponge FSM
--------------------------------------------------------------------------------

-- | Sponge phase: Absorbing message blocks or Squeezing output
data Phase = Absorbing | Squeezing | Idle
  deriving (Generic, NFDataX, Eq, Show)

-- Sequential (pipelined) sponge implementation
-- State machine for sequential Keccak-f[200] sponge
data SpongeState = SpongeState
  { stateData :: BitVector 200,
    roundCounter :: Index 18,
    active :: Bool -- True when processing rounds, False when idle
  }
  deriving (Generic, NFDataX)

--------------------------------------------------------------------------------
-- SHA3 Multi-Block Sequential FSM
--------------------------------------------------------------------------------

-- | State for SHA3 multi-block absorb/squeeze FSM
data SHA3State maxBlocks = SHA3State
  { sha3StateData :: BitVector 200,
    sha3RoundCounter :: Index 18,
    sha3Phase :: Phase,
    sha3Active :: Bool,
    sha3BlocksRemaining :: Unsigned 8, -- Use Unsigned to avoid Index overflow
    sha3SqueezedBits :: Unsigned 8, -- Track squeezed output bits
    sha3LatchedBlocks :: Vec maxBlocks (BitVector Rate) -- Latch blocks on start pulse
  }
  deriving (Generic, NFDataX)

-- | SHA3-f[200] sequential sponge with multi-block absorb and squeeze.
--
-- = Inputs
--
-- * @blocks@ - Vec of pre-padded rate blocks to absorb
-- * @start@ - Pulse to begin hashing (loads blocks, starts absorb)
--
-- = Outputs
--
-- * @digestValid@ - High when digest is ready
-- * @digest@ - DigestBits output
--
-- = Timing
--
-- * Load phase (1 cycle): Accept blocks vector, enter Absorbing
-- * Absorb phase (numBlocks × 19 cycles): For each block, absorb (1 cycle) + permute (18 rounds)
-- * Squeeze phase (⌈DigestBits/Rate⌉ × 19 cycles): Extract Rate bits per permutation until DigestBits collected
--
-- = Example (1 block, digest < rate)
--
-- @
-- Cycle 0: start=1, load blocks
-- Cycle 1: Absorb block 0, round 0
-- Cycles 2-18: Rounds 1-17
-- Cycle 19: Round 17 complete, enter Squeezing
-- Cycle 20: Extract digest (DigestBits < Rate, done in 1 cycle)
-- Cycle 21: digestValid=1, digest output
-- @
sha3f200Seq ::
  forall dom maxBlocks.
  ( HiddenClockResetEnable dom,
    KnownNat maxBlocks,
    1 <= maxBlocks
  ) =>
  Signal dom Bool ->
  Signal dom (Vec maxBlocks (BitVector Rate)) ->
  Signal dom (Bool, BitVector DigestBits)
sha3f200Seq start blocks = mealy step initialState (bundle (start, blocks))
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

    step ::
      SHA3State maxBlocks ->
      (Bool, Vec maxBlocks (BitVector Rate)) ->
      (SHA3State maxBlocks, (Bool, BitVector DigestBits))
    step st (startPulse, blockVec) =
      let currentPhase = sha3Phase st
          currentRound = sha3RoundCounter st

          -- Load blocks when start pulse arrives in Idle phase
          -- CRITICAL: Reset all state to zero for new message, latch input blocks
          stateAfterLoad
            | currentPhase == Idle && startPulse =
                let totalBlocks = natToNum @maxBlocks :: Int
                 in st
                      { sha3StateData = 0, -- Reset state to all zeros for new hash
                        sha3RoundCounter = 0, -- Reset round counter
                        sha3Active = False, -- Start idle, will activate on first absorb
                        sha3Phase = Absorbing,
                        sha3BlocksRemaining = fromIntegral (totalBlocks - 1), -- Will absorb first block immediately
                        sha3SqueezedBits = 0,
                        sha3LatchedBlocks = blockVec -- Latch input blocks
                      }
            | otherwise = st

          -- Absorb next block if in Absorbing phase and idle
          -- Use latched blocks, not live input bus
          stateAfterAbsorb
            | sha3Phase stateAfterLoad == Absorbing && not (sha3Active stateAfterLoad) =
                let totalBlocks = natToNum @maxBlocks :: Int
                    blocksLeft = fromIntegral (sha3BlocksRemaining stateAfterLoad) :: Int
                    blockIdx = totalBlocks - blocksLeft - 1
                    block = sha3LatchedBlocks stateAfterLoad !! blockIdx -- Use latched blocks!
                 in stateAfterLoad
                      { sha3StateData = sha3StateData stateAfterLoad `xor` ((0 :: BitVector Capacity) ++# block),
                        sha3Active = True,
                        sha3RoundCounter = 0
                      }
            | otherwise = stateAfterLoad

          -- Execute round if active
          stateData' =
            if sha3Active stateAfterAbsorb
              then keccakF200Round (resize (sha3RoundCounter stateAfterAbsorb)) (sha3StateData stateAfterAbsorb)
              else sha3StateData stateAfterAbsorb

          -- Advance round counter
          nextRound
            | sha3Active stateAfterAbsorb && currentRound == 17 = 0
            | sha3Active stateAfterAbsorb = currentRound + 1
            | otherwise = currentRound

          permutationComplete = sha3Active stateAfterAbsorb && currentRound == 17

          -- Use updated values from stateAfterAbsorb for phase transitions
          blocksLeft = sha3BlocksRemaining stateAfterAbsorb
          squeezedBits = sha3SqueezedBits stateAfterAbsorb

          -- Check if digest fits in current state (after this permutation completes)
          -- For our parameters: DigestBits=128, Rate=144, so digest always fits in one block
          digestFitsInOneBlock = natToNum @DigestBits <= natToNum @Rate

          -- Phase transitions (use Unsigned arithmetic to avoid Index overflow)
          (nextPhase, nextActive, nextBlocksLeft, nextSqueezed) =
            case (sha3Phase stateAfterAbsorb, permutationComplete) of
              (Absorbing, True) | blocksLeft == 0 ->
                -- All blocks absorbed, we now have a complete permuted state
                -- Check if digest fits in one rate block
                if digestFitsInOneBlock
                  then (Idle, False, 0, fromIntegral (natToNum @Rate)) -- Digest complete, go idle
                  else (Squeezing, True, 0, fromIntegral (natToNum @Rate)) -- Need more squeezing, stay active
              (Absorbing, True) ->
                (Absorbing, False, blocksLeft - 1, 0) -- More blocks to absorb
              (Squeezing, True) ->
                -- Additional squeeze permutation completed
                let newSqueezed = squeezedBits + fromIntegral (natToNum @Rate)
                 in if newSqueezed >= fromIntegral (natToNum @DigestBits)
                      then (Idle, False, 0, 0) -- Digest complete
                      else (Squeezing, True, 0, newSqueezed) -- Need more squeeze, run another permutation
              (_, False) ->
                (sha3Phase stateAfterAbsorb, sha3Active stateAfterAbsorb, blocksLeft, squeezedBits)
              _ ->
                (sha3Phase stateAfterAbsorb, sha3Active stateAfterAbsorb, blocksLeft, squeezedBits)

          -- Extract digest when transitioning to Idle from Absorbing or Squeezing
          digestReady = permutationComplete && (sha3Phase stateAfterAbsorb == Absorbing && blocksLeft == 0 || sha3Phase stateAfterAbsorb == Squeezing) && nextPhase == Idle
          digestOut = leToPlusKN @DigestBits @Rate truncateB (truncateB @_ @Rate stateData')

          nextState =
            SHA3State
              { sha3StateData = stateData',
                sha3RoundCounter = nextRound,
                sha3Phase = nextPhase,
                sha3Active = nextActive,
                sha3BlocksRemaining = nextBlocksLeft,
                sha3SqueezedBits = nextSqueezed,
                sha3LatchedBlocks = sha3LatchedBlocks stateAfterAbsorb -- Preserve latched blocks
              }
       in (nextState, (digestReady, digestOut))

-- | SHA3-f[200] top entity: hash arbitrary-length messages
--
-- = Ports
--
-- * CLK, RST, EN - Clock, reset, enable
-- * MSG_START - Pulse to begin hashing
-- * MSG_DATA - Message input (currently fixed 140 bits, will be padded to 1 block)
-- * DIGEST_VALID - High when digest is ready
-- * DIGEST_DATA - 128-bit digest output
--
-- = Operation
--
-- 1. Assert MSG_START for 1 cycle with MSG_DATA containing the message
-- 2. Wait for DIGEST_VALID to go high (~20 cycles for 1-block message)
-- 3. Read DIGEST_DATA when DIGEST_VALID is asserted
--
-- = Notes
--
-- * This is a pedagogical 200-bit SHA3 variant, not standard SHA3-256
-- * Current version accepts fixed 140-bit messages (1 block after padding)
-- * Latency: 1 (load) + numBlocks×19 (absorb) + ⌈DigestBits/Rate⌉×19 (squeeze) cycles
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF200_SHA3",
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
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (BitVector 140) -> -- Fixed message size for now
  Signal System (Bool, BitVector DigestBits)
topEntity clk rst en msgStart msgData =
  withClockResetEnable clk rst en $
    sha3f200Seq @System @1 msgStart paddedBlocks
  where
    -- Pad each input message to 1 block
    paddedBlocks = fmap (padSHA3 @Rate @140 @1) msgData
