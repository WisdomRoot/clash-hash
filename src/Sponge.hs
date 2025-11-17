{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
  (Signal dom (Index rounds, BitVector b) -> Signal dom (BitVector b)) -> -- Permutation component
  Signal dom Bool -> -- s_axis_tvalid
  Signal dom (BitVector rate) -> -- s_axis_tdata
  Signal dom Bool -> -- s_axis_tlast
  Signal dom Bool -> -- m_axis_tready
  ( Signal dom Bool, -- s_axis_tready
    Signal dom Bool, -- m_axis_tvalid
    Signal dom (BitVector rate), -- m_axis_tdata
    Signal dom Bool -- m_axis_tlast
  )
spongeAxi suffix permutationComponent sAxisTValid sAxisTData sAxisTLast mAxisTReady =
  (sAxisTReady, mAxisTValid, mAxisTData, mAxisTLast)
  where
    -- FSM with feedback loop through permutation component
    (axiOutputs, permIn) = unbundle $ mealy step initialState (bundle (sAxisTValid, sAxisTData, sAxisTLast, mAxisTReady, stateDataAfterPerm))

    -- Permutation component instantiation
    stateDataAfterPerm = permutationComponent permIn

    -- Unpack AXI outputs
    (sAxisTReady, mAxisTValid, mAxisTData, mAxisTLast) = unbundle axiOutputs

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
      (Bool, BitVector rate, Bool, Bool, BitVector b) ->
      (SpongeState b rate digest rounds digestBlocks, ((Bool, Bool, BitVector rate, Bool), (Index rounds, BitVector b)))
    step st (sAxisTValid_in, sAxisTData_in, sAxisTLast_in, mAxisTReady_in, stateDataAfterPerm_in) =
      let -- Event detection - compute once, reuse everywhere
          haveInput =
            sAxisTValid_in
              && spongePhase st
              == Absorbing
              && not (spongeActive st)
              && not (spongeDigestPending st)
              && not (spongePadPending st)
          lastBeat = sAxisTLast_in
          canOutput = spongeDigestPending st && mAxisTReady_in
          permDone = spongeActive st && spongeRoundCounter st == maxRound

          -- Explicit mode encoding
          modeAbsorbIdle = spongePhase st == Absorbing && not (spongeActive st)
          modeAbsorbBusy = spongePhase st == Absorbing && spongeActive st
          modeSqueezeIdle = spongePhase st == Squeezing && not (spongeActive st)
          modeSqueezeBusy = spongePhase st == Squeezing && spongeActive st

          -- Single transition table
          (nextPhase, nextActive, latchCurrentBlock, startPerm, setDigestPending, decSqueeze, setSeenTLast) =
            case () of
              _
                | modeAbsorbIdle && haveInput ->
                    (Absorbing, True, False, True, False, False, False)
                | modeAbsorbIdle && spongePadPending st ->
                    (Absorbing, True, False, True, False, False, True)
                | modeAbsorbBusy && permDone && spongeSeenTLast st ->
                    (Squeezing, False, True, False, True, False, False)
                | modeAbsorbBusy && permDone ->
                    (Absorbing, False, False, False, spongeDigestPending st, False, False)
                | modeSqueezeIdle && canOutput && spongeSqueezeBlocksRemaining st == 0 ->
                    (Absorbing, False, False, False, False, False, False)
                | modeSqueezeIdle && canOutput ->
                    (Squeezing, True, False, True, False, True, False)
                | modeSqueezeBusy && permDone ->
                    (Squeezing, False, True, False, True, False, False)
                | otherwise ->
                    (spongePhase st, spongeActive st, False, False, spongeDigestPending st, False, spongeSeenTLast st)

          -- Use permutation result when active
          stateData' = if spongeActive st then stateDataAfterPerm_in else spongeStateData st

          -- Per-register enables and next values
          -- Permutation state (BitVector b)
          stateEn = modeAbsorbIdle && haveInput || spongePadPending st && modeAbsorbIdle
          nextStateIn =
            if spongePadPending st && modeAbsorbIdle
              then spongeStateData st `xor` ((0 :: BitVector (b - rate)) ++# spongePadBlock st)
              else spongeStateData st `xor` ((0 :: BitVector (b - rate)) ++# sAxisTData_in)
          nextStateData =
            if nextPhase == Absorbing && spongePhase st == Squeezing
              then 0 -- Reset when returning to absorbing
              else if stateEn then nextStateIn else stateData'

          -- Round counter
          roundEn = startPerm || spongeActive st && not permDone
          nextRound =
            if startPerm || modeSqueezeIdle && canOutput && spongeSqueezeBlocksRemaining st > 0
              then 0
              else spongeRoundCounter st + 1

          -- Pad flags/block
          padBlockEn = modeAbsorbIdle && haveInput && lastBeat
          nextPadBlock =
            if nextPhase == Absorbing && spongePhase st == Squeezing
              then 0 -- Reset when returning to absorbing
              else if padBlockEn then padBlock else spongePadBlock st

          nextPadPend =
            if nextPhase == Absorbing && spongePhase st == Squeezing
              then False -- Reset when returning to absorbing
              else (modeAbsorbIdle && haveInput) && lastBeat

          -- Squeeze bookkeeping
          squeezeEn = decSqueeze
          nextSqueeze =
            if squeezeEn
              then spongeSqueezeBlocksRemaining st - 1
              else
                if setSeenTLast
                  then totalDigestBlocks - 1 -- Initialize when entering squeeze
                  else spongeSqueezeBlocksRemaining st

          -- Current output block
          currentBlockFromState = resize stateData' :: BitVector rate
          latchBlockEn = latchCurrentBlock
          nextBlock =
            if latchBlockEn
              then currentBlockFromState
              else
                if nextPhase == Absorbing && spongePhase st == Squeezing
                  then 0 -- Reset when returning to absorbing
                  else spongeCurrentBlock st

          -- Digest pending flag (latch on set, clear on handshake)
          nextDigest
            | setDigestPending = True
            | canOutput = False
            | otherwise = spongeDigestPending st

          -- Prepare permutation input for next cycle
          -- CRITICAL: Must use updated state after absorption/padding, not old state
          permutationInput =
            ( if roundEn then nextRound else spongeRoundCounter st,
              if stateEn then nextStateIn else spongeStateData st
            )

          -- AXI4-Stream outputs (pure combinational from control state)
          sAxisTReady_out = modeAbsorbIdle
          mAxisTValid_out = spongeDigestPending st
          mAxisTData_out = if spongeDigestPending st then spongeCurrentBlock st else 0
          mAxisTLast_out = spongeDigestPending st && spongeSqueezeBlocksRemaining st == 0

          nextState =
            SpongeState
              { spongeStateData = nextStateData,
                spongeRoundCounter = if roundEn then nextRound else spongeRoundCounter st,
                spongePhase = nextPhase,
                spongeActive = nextActive,
                spongeSeenTLast = setSeenTLast,
                spongeDigestPending = nextDigest,
                spongeSqueezeBlocksRemaining = nextSqueeze,
                spongeCurrentBlock = nextBlock,
                spongePadPending = nextPadPend,
                spongePadBlock = nextPadBlock
              }
       in (nextState, ((sAxisTReady_out, mAxisTValid_out, mAxisTData_out, mAxisTLast_out), permutationInput))

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
