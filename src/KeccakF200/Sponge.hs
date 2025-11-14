{-# LANGUAGE TypeApplications #-}

module KeccakF200.Sponge
  ( -- * SHA3-f[200] parameters
    Rate,
    Capacity,
    DigestBits,
    -- * FSM types
    Phase (..),
    SHA3State (..),
    -- * Top entity
    topEntity,
  )
where

import Clash.Prelude
import qualified KeccakF200.Permutation as Perm

--------------------------------------------------------------------------------
-- SHA3-f[200] Parameters
--------------------------------------------------------------------------------

-- | Concrete parameters for SHA3-f[200] (pedagogical 200-bit variant)
type Rate = 128          -- Rate: bits XORed per absorb/squeeze
type Capacity = 72       -- Capacity: 200 - Rate = 72
type DigestBits = 128    -- Output digest length

--------------------------------------------------------------------------------
-- AXI4-Stream SHA3 Sponge
--------------------------------------------------------------------------------
--
-- = Padding Expectations
--
-- This module expects pre-padded Rate-bit blocks from upstream.
-- Upstream logic must apply SHA3 pad10*1 with domain suffix (0b01):
--
-- 1. Append 0b01 suffix after message
-- 2. Append '1' bit (pad start)
-- 3. Append zeros to fill to Rate boundary
-- 4. Set final bit to '1' (pad end)
--
-- Example: 120-bit message, Rate=128
--   Input block: [message(120) | 01 | 1 | 00000 | 1] = 128 bits
--
-- Mark the final padded block with s_axis_tlast=1.

--------------------------------------------------------------------------------
-- SHA3 Multi-Block Sequential FSM
--------------------------------------------------------------------------------

-- | Sponge phase: Absorbing message blocks or Squeezing output
data Phase = Absorbing | Squeezing | Idle
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NFDataX)

-- | State for AXI4-Stream SHA3 FSM
data SHA3State = SHA3State
  { sha3StateData :: BitVector 200,
    sha3RoundCounter :: Index 18,
    sha3Phase :: Phase,
    sha3Active :: Bool,
    sha3SeenTLast :: Bool, -- Have we seen TLAST on input stream?
    sha3DigestPending :: Bool, -- Digest ready, waiting for m_axis_tready
    sha3DigestData :: BitVector DigestBits -- Latched digest output
  }
  deriving stock (Generic)
  deriving anyclass (NFDataX)

-- | SHA3-f[200] with AXI4-Stream interface.
--
-- = AXI4-Stream Protocol
--
-- * Input (slave): s_axis_tvalid, s_axis_tdata (Rate bits), s_axis_tlast, s_axis_tready (output)
-- * Output (master): m_axis_tvalid (output), m_axis_tdata (Rate bits), m_axis_tlast (output), m_axis_tready
--
-- = Operation
--
-- 1. Absorb: Accept pre-padded Rate-bit blocks when s_axis_tvalid && s_axis_tready
--    - XOR each block into state, run 18-round permutation
--    - s_axis_tlast marks final block, triggers transition to squeezing
--    - s_axis_tready deasserts during permutation (backpressure)
--
-- 2. Squeeze: Output DigestBits in Rate-bit chunks
--    - Drive m_axis_tvalid with digest in m_axis_tdata LSBs (zero-pad MSBs)
--    - m_axis_tlast asserted on final beat (always first beat for DigestBits <= Rate)
--    - Hold data stable until m_axis_tready asserted, then return to Idle
--
-- = Timing
--
-- * Per block: 1 cycle absorb + 18 cycles permutation = 19 cycles
-- * Digest output: 1 cycle (held until m_axis_tready)
sha3f200Seq ::
  forall dom.
  HiddenClockResetEnable dom =>
  Signal dom Bool -> -- s_axis_tvalid
  Signal dom (BitVector Rate) -> -- s_axis_tdata
  Signal dom Bool -> -- s_axis_tlast
  Signal dom Bool -> -- m_axis_tready
  ( Signal dom Bool, -- s_axis_tready
    Signal dom Bool, -- m_axis_tvalid
    Signal dom (BitVector Rate), -- m_axis_tdata
    Signal dom Bool -- m_axis_tlast
  )
sha3f200Seq sAxisTValid sAxisTData sAxisTLast mAxisTReady =
  (sAxisTReady, mAxisTValid, mAxisTData, mAxisTLast)
  where
    (sAxisTReady, mAxisTValid, mAxisTData, mAxisTLast) =
      unbundle $ mealy step initialState (bundle (sAxisTValid, sAxisTData, sAxisTLast, mAxisTReady))

    initialState =
      SHA3State
        { sha3StateData = 0,
          sha3RoundCounter = 0,
          sha3Phase = Absorbing, -- Start in Absorbing to accept input immediately
          sha3Active = False,
          sha3SeenTLast = False,
          sha3DigestPending = False,
          sha3DigestData = 0
        }

    step ::
      SHA3State ->
      (Bool, BitVector Rate, Bool, Bool) ->
      (SHA3State, (Bool, Bool, BitVector Rate, Bool))
    step st (sAxisTValid, sAxisTData, sAxisTLast, mAxisTReady) =
      let currentPhase = sha3Phase st
          currentRound = sha3RoundCounter st
          active = sha3Active st
          seenTLast = sha3SeenTLast st
          digestPending = sha3DigestPending st

          -- AXI4-Stream handshake: transfer occurs when both valid and ready
          inputTransfer = sAxisTValid && not active && currentPhase == Absorbing && not digestPending
          outputTransfer = digestPending && mAxisTReady

          -- Absorb block on valid input transfer
          stateAfterAbsorb
            | inputTransfer =
                st
                  { sha3StateData = sha3StateData st `xor` ((0 :: BitVector Capacity) ++# sAxisTData),
                    sha3Active = True,
                    sha3RoundCounter = 0,
                    sha3SeenTLast = seenTLast || sAxisTLast
                  }
            | otherwise = st

          -- Execute permutation round if active
          stateData' =
            if sha3Active stateAfterAbsorb
              then Perm.topEntity (resize (sha3RoundCounter stateAfterAbsorb), sha3StateData stateAfterAbsorb)
              else sha3StateData stateAfterAbsorb

          -- Advance round counter
          nextRound
            | sha3Active stateAfterAbsorb && currentRound == 17 = 0
            | sha3Active stateAfterAbsorb = currentRound + 1
            | otherwise = currentRound

          permutationComplete = sha3Active stateAfterAbsorb && currentRound == 17

          -- Phase transitions based on permutation completion
          (nextPhase, nextActive, nextSeenTLast, nextDigestPending, nextDigestData) =
            case (sha3Phase stateAfterAbsorb, permutationComplete, sha3SeenTLast stateAfterAbsorb) of
              -- Absorb complete with TLAST seen: transition to squeezing, latch digest
              (Absorbing, True, True) ->
                let digest = leToPlusKN @DigestBits @Rate truncateB (truncateB @_ @Rate stateData')
                 in (Squeezing, False, False, True, digest)
              -- Absorb complete but no TLAST: keep absorbing
              (Absorbing, True, False) ->
                (Absorbing, False, False, digestPending, sha3DigestData stateAfterAbsorb)
              -- Digest output handshake complete: reset state and return to Absorbing for next message
              _ | outputTransfer ->
                (Absorbing, False, False, False, 0)
              -- No state change
              _ ->
                (sha3Phase stateAfterAbsorb, sha3Active stateAfterAbsorb, sha3SeenTLast stateAfterAbsorb, digestPending, sha3DigestData stateAfterAbsorb)

          -- AXI4-Stream output signals
          sAxisTReady_out = not active && currentPhase == Absorbing && not digestPending
          mAxisTValid_out = digestPending
          mAxisTData_out =
            if digestPending
              then (0 :: BitVector (Rate - DigestBits)) ++# sha3DigestData stateAfterAbsorb
              else 0
          mAxisTLast_out = digestPending -- Always true when digest is ready (single beat)

          -- Reset state data when digest output completes
          nextStateData = if outputTransfer then 0 else stateData'

          nextState =
            SHA3State
              { sha3StateData = nextStateData,
                sha3RoundCounter = nextRound,
                sha3Phase = nextPhase,
                sha3Active = nextActive,
                sha3SeenTLast = nextSeenTLast,
                sha3DigestPending = nextDigestPending,
                sha3DigestData = nextDigestData
              }
       in (nextState, (sAxisTReady_out, mAxisTValid_out, mAxisTData_out, mAxisTLast_out))

-- | SHA3-f[200] AXI4-Stream top entity
--
-- = Ports
--
-- * CLK, RST, EN - Clock, reset, enable
-- * S_AXIS_TVALID - Input valid (source asserts when data ready)
-- * S_AXIS_TDATA - Input data (Rate=128 bits, pre-padded blocks)
-- * S_AXIS_TLAST - Input last (marks final block of message)
-- * S_AXIS_TREADY - Input ready (sink asserts when ready to accept, output)
-- * M_AXIS_TVALID - Output valid (asserted when digest ready, output)
-- * M_AXIS_TDATA - Output data (Rate=128 bits, digest in LSBs, zero-padded)
-- * M_AXIS_TLAST - Output last (always high for digest beat, output)
-- * M_AXIS_TREADY - Output ready (downstream asserts when ready to accept)
--
-- = AXI4-Stream Protocol
--
-- * Input: Source drives TVALID/TDATA/TLAST, sink drives TREADY
--   - Transfer occurs when both TVALID and TREADY are high
--   - Source must hold TVALID/TDATA/TLAST stable until TREADY asserted
--   - TLAST marks the final pre-padded block
--
-- * Output: This module drives TVALID/TDATA/TLAST, downstream drives TREADY
--   - Digest held stable until TREADY acknowledged
--   - TLAST always high (single-beat digest output)
--
-- = Timing
--
-- * Per block: 1 cycle absorb + 18 cycles permutation = 19 cycles
-- * Output: Digest held until downstream ready
--
-- = Notes
--
-- * Expects pre-padded Rate-bit blocks from upstream (pad10*1 + SHA3 suffix)
-- * This is a pedagogical 200-bit SHA3 variant, not standard SHA3-256
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF200_SHA3_AXI",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "S_AXIS_TVALID",
            PortName "S_AXIS_TDATA",
            PortName "S_AXIS_TLAST",
            PortName "M_AXIS_TREADY"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "S_AXIS_TREADY",
              PortName "M_AXIS_TVALID",
              PortName "M_AXIS_TDATA",
              PortName "M_AXIS_TLAST"
            ]
      }
  )
  #-}
{-# OPAQUE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool -> -- S_AXIS_TVALID
  Signal System (BitVector Rate) -> -- S_AXIS_TDATA
  Signal System Bool -> -- S_AXIS_TLAST
  Signal System Bool -> -- M_AXIS_TREADY
  ( Signal System Bool, -- S_AXIS_TREADY
    Signal System Bool, -- M_AXIS_TVALID
    Signal System (BitVector Rate), -- M_AXIS_TDATA
    Signal System Bool -- M_AXIS_TLAST
  )
topEntity clk rst en sAxisTValid sAxisTData sAxisTLast mAxisTReady =
  withClockResetEnable clk rst en $
    sha3f200Seq sAxisTValid sAxisTData sAxisTLast mAxisTReady
