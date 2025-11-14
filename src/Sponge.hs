{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
data SpongeState b rate digest rounds digestBlocks = SpongeState
  { spongeStateData :: BitVector b,
    spongeRoundCounter :: Index rounds,
    spongePhase :: Phase,
    spongeActive :: Bool,
    spongeSeenTLast :: Bool,
    spongeDigestPending :: Bool,
    spongeSqueezeBlocksRemaining :: Index digestBlocks,
    spongeCurrentBlock :: BitVector rate,
    spongePadPending :: Bool,
    spongePadBlock :: BitVector rate
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
-- Accepts raw input blocks on AXI stream. TLAST marks final full-width data block.
-- Automatically constructs and injects pad10*1 block after TLAST.
-- Outputs digest in rate-bit blocks. Supports multi-cycle squeezing when digest > rate.
--
-- = Domain Separation Suffix
--
-- The 2-bit suffix parameter specifies the domain separation:
-- * @0b01@ - SHA3 hash functions
-- * @0b11@ - SHAKE extendable-output functions
-- * @0b10@ - RawSHAKE (raw Keccak)
--
-- The padding block format is: suffix || 1 || 0...0 || 1 (pad10*1 rule)
spongeAxi ::
  forall dom b rate digest rounds digestBlocks.
  ( HiddenClockResetEnable dom,
    KnownNat b,
    KnownNat rate,
    KnownNat digest,
    KnownNat rounds,
    KnownNat digestBlocks,
    digestBlocks ~ DivRU digest rate,
    1 <= b,
    4 <= rate,
    1 <= digest,
    1 <= rounds,
    1 <= digestBlocks,
    rate <= b,
    digest <= b
  ) =>
  BitVector 2 -> -- Domain separation suffix (0b01=SHA3, 0b11=SHAKE, 0b10=RawSHAKE)
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
spongeAxi suffix permRound sAxisTValid sAxisTData sAxisTLast mAxisTReady =
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
          spongeSqueezeBlocksRemaining = 0,
          spongeCurrentBlock = 0,
          spongePadPending = False,
          spongePadBlock = 0
        }

    maxRound = maxBound :: Index rounds

    totalDigestBlocks = natToNum @digestBlocks :: Index digestBlocks

    padBlock :: BitVector rate
    padBlock = suffixPadBlock suffix

    step ::
      SpongeState b rate digest rounds digestBlocks ->
      (Bool, BitVector rate, Bool, Bool) ->
      (SpongeState b rate digest rounds digestBlocks, (Bool, Bool, BitVector rate, Bool))
    step st (sAxisTValid_in, sAxisTData_in, sAxisTLast_in, mAxisTReady_in) =
      let currentPhase = spongePhase st
          currentRound = spongeRoundCounter st
          active = spongeActive st
          digestPending = spongeDigestPending st
          squeezeBlocksRemaining = spongeSqueezeBlocksRemaining st
          padPending = spongePadPending st

          -- AXI4-Stream handshake
          inputTransfer = sAxisTValid_in && not active && currentPhase == Absorbing && not digestPending && not padPending
          padTransfer = padPending && not active && currentPhase == Absorbing && not digestPending
          outputTransfer = digestPending && mAxisTReady_in

          -- Absorb block on input transfer or pad transfer
          stateAfterAbsorb
            | inputTransfer =
                st
                  { spongeStateData = spongeStateData st `xor` ((0 :: BitVector (b - rate)) ++# sAxisTData_in),
                    spongeActive = True,
                    spongeRoundCounter = 0,
                    spongePadPending = sAxisTLast_in,
                    spongePadBlock = if sAxisTLast_in then padBlock else spongePadBlock st
                  }
            | padTransfer =
                st
                  { spongeStateData = spongeStateData st `xor` ((0 :: BitVector (b - rate)) ++# spongePadBlock st),
                    spongeActive = True,
                    spongeRoundCounter = 0,
                    spongePadPending = False,
                    spongeSeenTLast = True
                  }
            | otherwise = st

          -- Execute permutation round if active
          stateData' =
            if spongeActive stateAfterAbsorb
              then permRound (spongeRoundCounter stateAfterAbsorb) (spongeStateData stateAfterAbsorb)
              else spongeStateData stateAfterAbsorb

          -- Advance round counter (reset when starting new permutation)
          nextRound
            | spongeActive stateAfterAbsorb && currentRound == maxRound = 0
            | spongeActive stateAfterAbsorb = currentRound + 1
            -- Reset when starting squeeze permutation after output
            | currentPhase == Squeezing && outputTransfer && squeezeBlocksRemaining > 0 = 0
            | otherwise = currentRound

          permutationComplete = spongeActive stateAfterAbsorb && currentRound == maxRound

          -- Extract current rate-sized block from state LSBs
          currentBlockFromState = resize stateData' :: BitVector rate

          -- Phase transitions and squeeze block management
          (nextPhase, nextActive, nextSeenTLast, nextDigestPending, nextSqueezeBlocks, nextCurrentBlock) =
            case (spongePhase stateAfterAbsorb, permutationComplete, spongeSeenTLast stateAfterAbsorb) of
              -- Absorb complete with TLAST seen: enter squeezing, prepare first block
              (Absorbing, True, True) ->
                (Squeezing, False, False, True, totalDigestBlocks - 1, currentBlockFromState)
              -- Absorb complete but no TLAST: keep absorbing
              (Absorbing, True, False) ->
                (Absorbing, False, False, digestPending, squeezeBlocksRemaining, spongeCurrentBlock stateAfterAbsorb)
              -- Squeezing: output block accepted
              _ | currentPhase == Squeezing && outputTransfer ->
                if squeezeBlocksRemaining == 0
                  then -- Last block sent, return to absorbing
                    (Absorbing, False, False, False, 0, 0)
                  else -- More blocks needed, start next permutation
                    (Squeezing, True, False, False, squeezeBlocksRemaining - 1, 0)
              -- Squeezing: permutation complete, latch next block
              _ | currentPhase == Squeezing && permutationComplete ->
                (Squeezing, False, False, True, squeezeBlocksRemaining, currentBlockFromState)
              -- No state change
              _ ->
                (spongePhase stateAfterAbsorb, spongeActive stateAfterAbsorb, spongeSeenTLast stateAfterAbsorb, digestPending, squeezeBlocksRemaining, spongeCurrentBlock stateAfterAbsorb)

          -- AXI4-Stream outputs
          sAxisTReady_out = not active && currentPhase == Absorbing && not digestPending && not padPending
          mAxisTValid_out = digestPending
          mAxisTData_out = if digestPending then spongeCurrentBlock stateAfterAbsorb else 0
          mAxisTLast_out = digestPending && squeezeBlocksRemaining == 0

          -- Reset state data when returning to absorbing
          nextStateData = if nextPhase == Absorbing && currentPhase == Squeezing then 0 else stateData'

          -- Reset padding state when transitioning back to Absorbing
          (nextPadPending, nextPadBlock) =
            if nextPhase == Absorbing && currentPhase == Squeezing
              then (False, 0)
              else (spongePadPending stateAfterAbsorb, spongePadBlock stateAfterAbsorb)

          nextState =
            SpongeState
              { spongeStateData = nextStateData,
                spongeRoundCounter = nextRound,
                spongePhase = nextPhase,
                spongeActive = nextActive,
                spongeSeenTLast = nextSeenTLast,
                spongeDigestPending = nextDigestPending,
                spongeSqueezeBlocksRemaining = nextSqueezeBlocks,
                spongeCurrentBlock = nextCurrentBlock,
                spongePadPending = nextPadPending,
                spongePadBlock = nextPadBlock
              }
       in (nextState, (sAxisTReady_out, mAxisTValid_out, mAxisTData_out, mAxisTLast_out))

    suffixPadBlock :: BitVector 2 -> BitVector rate
    suffixPadBlock suff =
      let suffixBits :: BitVector rate
          suffixBits = setBitIf (testBit suff 0) 1 $ setBitIf (testBit suff 1) 0 (0 :: BitVector rate)
          firstPadBit = setBit (0 :: BitVector rate) 2
          finalPadBit = setBit (0 :: BitVector rate) (rateWidth - 1)
       in suffixBits .|. firstPadBit .|. finalPadBit

    rateWidth :: Int
    rateWidth = natToNum @rate

    setBitIf :: Bool -> Int -> BitVector rate -> BitVector rate
    setBitIf cond idx bv = if cond then setBit bv idx else bv
