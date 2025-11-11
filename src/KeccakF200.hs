{-# LANGUAGE TypeApplications #-}

module KeccakF200
  ( thetaF200,
    rhoF200,
    piF200,
    chiF200,
    iotaF200,
    keccakF200Sponge,
    keccakF200SpongeSeq,
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants
import Data.Maybe (fromMaybe, isJust)
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

-- {-# ANN
--   topEntity
--   ( Synthesize
--       { t_name = "KeccakF200_Sponge",
--         t_inputs =
--           [ PortName "CLK",
--             PortName "RST",
--             PortName "EN",
--             PortName "DIN"
--           ],
--         t_output = PortName "DOUT"
--       }
--   )
--   #-}
-- {-# OPAQUE topEntity #-}
-- topEntity ::
--   Clock System ->
--   Reset System ->
--   Enable System ->
--   Signal System (BitVector MessageBits) ->
--   Signal System (BitVector OutputBits)
-- topEntity = exposeClockResetEnable $ fmap spongeStep
--   where
--     spongeStep :: BitVector MessageBits -> BitVector OutputBits
--     spongeStep = keccakF200Sponge @Rate @NumAbsorbBlocks @MessageBits @NumSqueezeBlocks @OutputBits

--------------------------------------------------------------------------------

-- Concrete parameters for synthesis
type Rate = 144

-- Sequential (pipelined) sponge implementation
-- State machine for sequential Keccak-f[200] sponge
data SpongeState = SpongeState
  { stateData :: BitVector 200,
    roundCounter :: Index 18,
    active :: Bool -- True when processing rounds, False when idle
  }
  deriving (Generic, NFDataX)

-- | Sequential Keccak-f[200] sponge that pipelines rounds across cycles.
--
-- = Behavior
--
-- * Absorbs one rate block on one cycle, then applies one round per cycle for 18 cycles, then outputs.
-- * Input: @Signal dom (Maybe (BitVector r))@ - @Just block@ to absorb, @Nothing@ when idle
-- * Output: @Signal dom (Maybe (BitVector r))@ - @Just result@ on cycle 19 after absorb, @Nothing@ otherwise
--
-- = State Machine
--
-- * __Idle__ (@active = False@): Ready to accept new block. State holds constant, no rounds execute.
-- * __Absorbing__ (cycle when @Just block@ arrives while idle): XOR block into low @r@ bits, set @active = True@, reset @roundCounter = 0@. On this same cycle, round 0 executes.
-- * __Processing__ (@active = True@, cycles 2-18 after absorb): Execute one round per cycle (rounds 1-17), increment @roundCounter@.
-- * __Completing__ (@active = True@, cycle 18): Execute round 17, output @Just result@, set @active = False@ for next cycle.
--
-- = Properties
--
-- [No spurious outputs on reset] Output remains @Nothing@ until first real block completes 18 rounds.
--
-- [State frozen when idle] Permutation only executes when @active = True@; idle state holds last valid value.
--
-- [Gated absorb] New blocks only absorbed when @active = False@ (idle). Blocks arriving during processing are __silently dropped__.
--
-- [Deterministic results] Each accepted block receives exactly 18 rounds with no interruption or clobbering.
--
-- [No backpressure signal] Interface has no ready/valid handshake. Producer must ensure @Just block@ inputs are spaced ≥19 cycles apart.
--
-- = Example Timing
--
-- @
-- Cycle:  0   1   2   3  ...  18  19  20  21  22 ...  38  39
-- Input:  J   N   N   N  ...  N   N   J   N   N  ...  N   N
-- Active: T   T   T   T  ...  T   F   T   T   T  ...  T   F
-- Round:  0   1   2   3  ...  17  -   0   1   2  ...  17  -
-- Output: N   N   N   N  ...  N   J   N   N   N  ...  N   J
--
-- J = Just block/result, N = Nothing, T = True, F = False
-- Cycle 0: Absorb block, execute round 0
-- Cycle 1-17: Execute rounds 1-17
-- Cycle 18: Execute round 17, output result, deactivate
-- Cycle 19: Idle, ready for next block
-- @
keccakF200SpongeSeq ::
  forall dom r.
  (HiddenClockResetEnable dom, KnownNat r, r <= 200) =>
  Signal dom (Maybe (BitVector r)) ->
  Signal dom (Maybe (BitVector r))
keccakF200SpongeSeq input = output
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
                    { stateData = stateData st `xor` ((0 :: BitVector (200 - r)) ++# block),
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
              then keccakF200Round (resize currentRound) (stateData stateAfterAbsorb)
              else stateData stateAfterAbsorb

          nextRoundCounter
            | isActive && currentRound == 17 = 0
            | isActive = currentRound + 1
            | otherwise = currentRound

          -- PROPERTY: Deterministic results - each block gets exactly 18 rounds
          completedPermutation = isActive && currentRound == 17

          -- Extract output (low r bits) when permutation completes
          outputMaybe =
            if completedPermutation
              then Just (leToPlusKN @r @200 truncateB stateAfterRound)
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

-- | Sequential sponge top entity (one round per cycle, 18 cycles per block)
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF200_SpongeSeq",
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
    dout = withClockResetEnable clk rst en (keccakF200SpongeSeq @System @Rate) inputMaybe
