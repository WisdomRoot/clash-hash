{-# LANGUAGE TypeApplications #-}

module KeccakF1600
  ( thetaF1600,
    rhoF1600,
    piF1600,
    chiF1600,
    iotaF1600,
    keccakF1600Sponge,
    keccakF1600SpongeSeq,
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants
import Data.Maybe (fromMaybe, isJust)
import Sponge (SpongeParameter, sponge)

-- Theta transformation: XOR with column parities
thetaF1600 :: BitVector 1600 -> BitVector 1600
thetaF1600 bv =
  ifoldl
    ( \acc idx indices11 ->
        let bitOut = fold xor (map (bv !) indices11)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.theta 6)

-- Chi transformation expressed directly on BitVector
chiF1600 :: BitVector 1600 -> BitVector 1600
chiF1600 bv =
  ifoldl
    ( \acc idx (i0, i1, i2) ->
        let bitOut = bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.chi 6)

-- Pi transformation: bit permutation on BitVector
piF1600 :: BitVector 1600 -> BitVector 1600
piF1600 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.pi 6)

-- Rho transformation: bit permutation on BitVector (lane rotation)
rhoF1600 :: BitVector 1600 -> BitVector 1600
rhoF1600 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.rho 6)

iotaF1600 :: Index 24 -> BitVector 1600 -> BitVector 1600
iotaF1600 roundIdx bv =
  let lane0 = slice d63 d0 bv -- Extract first 64 bits (lane 0)
      lane0' = lane0 `xor` ($(Constants.iota) !! roundIdx) -- XOR with selected round constant (no truncation needed)
      upperBits = bv `shiftR` 64 :: BitVector 1600 -- Get bits 64-1599
   in (truncateB upperBits :: BitVector 1536) ++# lane0' -- Replace bits 0-63 with result

-- Complete Keccak-f[1600] round: Theta, Rho, Pi, Chi, Iota
keccakF1600Round :: Index 24 -> BitVector 1600 -> BitVector 1600
keccakF1600Round roundIdx =
  iotaF1600 roundIdx . chiF1600 . piF1600 . rhoF1600 . thetaF1600

-- | Full Keccak-f[1600] permutation: 24 rounds (12 + 2*l where l=6)
-- Applies all rounds in sequence using the round constants
keccakF1600 :: BitVector 1600 -> BitVector 1600
keccakF1600 initialState =
  foldl applyRound initialState (indicesI @24)
  where
    applyRound state roundIdx = keccakF1600Round roundIdx state

-- | Keccak-f[1600] sponge function with configurable rate/capacity.
-- Instantiation: r=1088, capacity=512, for SHA3-256
keccakF1600Sponge :: forall r n m k d. (SpongeParameter 1600 r n m k d) => BitVector m -> BitVector d
keccakF1600Sponge = sponge @1600 @r @n @m @k @d keccakF1600

--------------------------------------------------------------------------------

-- Concrete parameters for synthesis
type Rate = 1088

-- Sequential (pipelined) sponge implementation
-- State machine for sequential Keccak-f[1600] sponge
data SpongeState = SpongeState
  { stateData :: BitVector 1600,
    roundCounter :: Index 24,
    active :: Bool -- True when processing rounds, False when idle
  }
  deriving (Generic, NFDataX)

-- | Sequential Keccak-f[1600] sponge that pipelines rounds across cycles.
--
-- = Behavior
--
-- * Absorbs one rate block on one cycle, then applies one round per cycle for 24 cycles, then outputs.
-- * Input: @Signal dom (Maybe (BitVector r))@ - @Just block@ to absorb, @Nothing@ when idle
-- * Output: @Signal dom (Maybe (BitVector r))@ - @Just result@ on cycle 24 after absorb, @Nothing@ otherwise
--
-- = State Machine
--
-- * __Idle__ (@active = False@): Ready to accept new block. State holds constant, no rounds execute.
-- * __Absorbing__ (cycle when @Just block@ arrives while idle): XOR block into low @r@ bits, set @active = True@, reset @roundCounter = 0@. On this same cycle, round 0 executes.
-- * __Processing__ (@active = True@, cycles 2-24 after absorb): Execute one round per cycle (rounds 1-23), increment @roundCounter@.
-- * __Completing__ (@active = True@, cycle 24): Execute round 23, output @Just result@, set @active = False@ for next cycle.
--
-- = Properties
--
-- [No spurious outputs on reset] Output remains @Nothing@ until first real block completes 24 rounds.
--
-- [State frozen when idle] Permutation only executes when @active = True@; idle state holds last valid value.
--
-- [Gated absorb] New blocks only absorbed when @active = False@ (idle). Blocks arriving during processing are __silently dropped__.
--
-- [Deterministic results] Each accepted block receives exactly 24 rounds with no interruption or clobbering.
--
-- [No backpressure signal] Interface has no ready/valid handshake. Producer must ensure @Just block@ inputs are spaced ≥25 cycles apart.
--
-- = Example Timing
--
-- @
-- Cycle:  0   1   2   3  ...  24  25  26  27  28 ...  50  51
-- Input:  J   N   N   N  ...  N   N   J   N   N  ...  N   N
-- Active: T   T   T   T  ...  T   F   T   T   T  ...  T   F
-- Round:  0   1   2   3  ...  23  -   0   1   2  ...  23  -
-- Output: N   N   N   N  ...  N   J   N   N   N  ...  N   J
--
-- J = Just block/result, N = Nothing, T = True, F = False
-- Cycle 0: Absorb block, execute round 0
-- Cycle 1-23: Execute rounds 1-23
-- Cycle 24: Execute round 23, output result, deactivate
-- Cycle 25: Idle, ready for next block
-- @
keccakF1600SpongeSeq ::
  forall dom r.
  (HiddenClockResetEnable dom, KnownNat r, r <= 1600) =>
  Signal dom (Maybe (BitVector r)) ->
  Signal dom (Maybe (BitVector r))
keccakF1600SpongeSeq input = output
  where
    initialState =
      SpongeState
        { stateData = 0,
          roundCounter = 0,
          active = False
        }

    output = mealyB step initialState input

    step :: SpongeState -> Maybe (BitVector r) -> (SpongeState, Maybe (BitVector r))
    step st maybeBlock =
      let -- Only absorb new block when idle (not active)
          -- PROPERTY: Gated absorb prevents clobbering in-flight permutations
          canAbsorb = not (active st)

          -- Absorb phase: XOR block into low r bits if provided and idle, activate processing
          -- PROPERTY: Blocks arriving while active are silently dropped (no backpressure signal)
          stateAfterAbsorb = case maybeBlock of
            Just block
              | canAbsorb ->
                  st
                    { stateData = stateData st `xor` ((0 :: BitVector (1600 - r)) ++# block),
                      roundCounter = 0,
                      active = True
                    }
            _ -> st

          -- Only apply round and advance counter when active
          -- PROPERTY: State frozen when idle - no spurious outputs, no wasted power
          isActive = active stateAfterAbsorb
          currentRound = roundCounter stateAfterAbsorb

          stateAfterRound =
            if isActive
              then keccakF1600Round currentRound (stateData stateAfterAbsorb)
              else stateData stateAfterAbsorb

          nextRoundCounter
            | isActive && currentRound == 23 = 0
            | isActive = currentRound + 1
            | otherwise = currentRound

          -- PROPERTY: Deterministic results - each block gets exactly 24 rounds
          completedPermutation = isActive && currentRound == 23

          -- Extract output (low r bits) when permutation completes
          outputMaybe =
            if completedPermutation
              then Just (leToPlusKN @r @1600 truncateB stateAfterRound)
              else Nothing

          -- Deactivate after completing permutation
          nextActive = (not completedPermutation && isActive)

          nextState =
            SpongeState
              { stateData = stateAfterRound,
                roundCounter = nextRoundCounter,
                active = nextActive
              }
       in (nextState, outputMaybe)

-- | Sequential sponge top entity (one round per cycle, 24 cycles per block)
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF1600_SpongeSeq",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "DIN_VALID",
            PortName "DIN"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "DOUT_VALID",
              PortName "DOUT"
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
  Signal System (BitVector Rate) ->
  Signal System (Bool, BitVector Rate)
topEntity clk rst en valid din = bundle (isJust <$> dout, fromMaybe 0 <$> dout)
  where
    inputMaybe = (\v d -> if v then Just d else Nothing) <$> valid <*> din
    dout = withClockResetEnable clk rst en (keccakF1600SpongeSeq @System @Rate) inputMaybe
