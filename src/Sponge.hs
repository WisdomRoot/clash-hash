{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sponge
  ( -- * AXI4-Stream sponge
    spongeAxi,
  )
where

import Clash.Prelude

--------------------------------------------------------------------------------
-- AXI4-Stream Sponge
--------------------------------------------------------------------------------

-- | Sponge phase for AXI streaming
data Phase = Absorbing | Squeezing
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)

-- | State for generic AXI4-Stream sponge FSM
data SpongeState b rate digest rounds = SpongeState
  { spongeStateData :: BitVector b,
    spongeRoundCounter :: Index rounds,
    spongePhase :: Phase,
    spongeActive :: Bool,
    spongeSeenTLast :: Bool,
    spongeDigestPending :: Bool,
    spongeDigestData :: BitVector digest
  }
  deriving stock (Generic)
  deriving anyclass (NFDataX)

-- | Generic AXI4-Stream sponge construction.
--
-- = Parameters
--
-- * @b@ - Permutation width (state size)
-- * @rate@ - Rate (bits absorbed/squeezed per permutation)
-- * @digest@ - Output digest size in bits
-- * @rounds@ - Number of permutation rounds
--
-- = AXI4-Stream Interface
--
-- * Input (slave): s_axis_tvalid, s_axis_tdata (rate bits), s_axis_tlast, s_axis_tready (output)
-- * Output (master): m_axis_tvalid, m_axis_tdata (rate bits), m_axis_tlast, m_axis_tready
--
-- = Operation
--
-- Expects pre-padded rate-bit blocks on input stream. TLAST marks final block.
-- Outputs digest in LSBs of m_axis_tdata with MSBs zero-padded.
spongeAxi ::
  forall dom b rate digest rounds.
  ( HiddenClockResetEnable dom,
    KnownNat b,
    KnownNat rate,
    KnownNat digest,
    KnownNat rounds,
    1 <= b,
    1 <= rate,
    1 <= digest,
    1 <= rounds,
    rate <= b,
    digest <= rate,
    digest <= b
  ) =>
  (Index rounds -> BitVector b -> BitVector b) -> -- Permutation round function
  Signal dom Bool -> -- s_axis_tvalid
  Signal dom (BitVector rate) -> -- s_axis_tdata
  Signal dom Bool -> -- s_axis_tlast
  Signal dom Bool -> -- m_axis_tready
  ( Signal dom Bool, -- s_axis_tready
    Signal dom Bool, -- m_axis_tvalid
    Signal dom (BitVector rate), -- m_axis_tdata
    Signal dom Bool -- m_axis_tlast
  )
spongeAxi permRound sAxisTValid sAxisTData sAxisTLast mAxisTReady =
  (sAxisTReady, mAxisTValid, mAxisTData, mAxisTLast)
  where
    (sAxisTReady, mAxisTValid, mAxisTData, mAxisTLast) =
      unbundle $ mealy step initialState (bundle (sAxisTValid, sAxisTData, sAxisTLast, mAxisTReady))

    initialState =
      SpongeState
        { spongeStateData = 0,
          spongeRoundCounter = 0,
          spongePhase = Absorbing,
          spongeActive = False,
          spongeSeenTLast = False,
          spongeDigestPending = False,
          spongeDigestData = 0
        }

    maxRound = maxBound :: Index rounds

    step ::
      SpongeState b rate digest rounds ->
      (Bool, BitVector rate, Bool, Bool) ->
      (SpongeState b rate digest rounds, (Bool, Bool, BitVector rate, Bool))
    step st (sAxisTValid_in, sAxisTData_in, sAxisTLast_in, mAxisTReady_in) =
      let currentPhase = spongePhase st
          currentRound = spongeRoundCounter st
          active = spongeActive st
          seenTLast = spongeSeenTLast st
          digestPending = spongeDigestPending st

          -- AXI4-Stream handshake
          inputTransfer = sAxisTValid_in && not active && currentPhase == Absorbing && not digestPending
          outputTransfer = digestPending && mAxisTReady_in

          -- Absorb block on input transfer
          stateAfterAbsorb
            | inputTransfer =
                st
                  { spongeStateData = spongeStateData st `xor` ((0 :: BitVector (b - rate)) ++# sAxisTData_in),
                    spongeActive = True,
                    spongeRoundCounter = 0,
                    spongeSeenTLast = seenTLast || sAxisTLast_in
                  }
            | otherwise = st

          -- Execute permutation round if active
          stateData' =
            if spongeActive stateAfterAbsorb
              then permRound (spongeRoundCounter stateAfterAbsorb) (spongeStateData stateAfterAbsorb)
              else spongeStateData stateAfterAbsorb

          -- Advance round counter
          nextRound
            | spongeActive stateAfterAbsorb && currentRound == maxRound = 0
            | spongeActive stateAfterAbsorb = currentRound + 1
            | otherwise = currentRound

          permutationComplete = spongeActive stateAfterAbsorb && currentRound == maxRound

          -- Phase transitions
          (nextPhase, nextActive, nextSeenTLast, nextDigestPending, nextDigestData) =
            case (spongePhase stateAfterAbsorb, permutationComplete, spongeSeenTLast stateAfterAbsorb) of
              -- Absorb complete with TLAST seen: latch digest from LSBs of state
              (Absorbing, True, True) ->
                let digest = resize stateData' :: BitVector digest
                 in (Squeezing, False, False, True, digest)
              -- Absorb complete but no TLAST: keep absorbing
              (Absorbing, True, False) ->
                (Absorbing, False, False, digestPending, spongeDigestData stateAfterAbsorb)
              -- Digest output complete: reset and return to Absorbing
              _
                | outputTransfer ->
                    (Absorbing, False, False, False, 0)
              -- No state change
              _ ->
                (spongePhase stateAfterAbsorb, spongeActive stateAfterAbsorb, spongeSeenTLast stateAfterAbsorb, digestPending, spongeDigestData stateAfterAbsorb)

          -- AXI4-Stream outputs
          sAxisTReady_out = not active && currentPhase == Absorbing && not digestPending
          mAxisTValid_out = digestPending
          mAxisTData_out =
            if digestPending
              then (0 :: BitVector (rate - digest)) ++# spongeDigestData stateAfterAbsorb
              else 0
          mAxisTLast_out = digestPending

          -- Reset state data when digest output completes
          nextStateData = if outputTransfer then 0 else stateData'

          nextState =
            SpongeState
              { spongeStateData = nextStateData,
                spongeRoundCounter = nextRound,
                spongePhase = nextPhase,
                spongeActive = nextActive,
                spongeSeenTLast = nextSeenTLast,
                spongeDigestPending = nextDigestPending,
                spongeDigestData = nextDigestData
              }
       in (nextState, (sAxisTReady_out, mAxisTValid_out, mAxisTData_out, mAxisTLast_out))
