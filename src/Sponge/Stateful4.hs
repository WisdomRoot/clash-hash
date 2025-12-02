{-# LANGUAGE TypeApplications #-}

module Sponge.Stateful4
  ( -- * Stateful4 Sponge
    stateful4Sponge,
  )
where

import Clash.Prelude
import Hash.Combinational qualified
import Sponge.Stateful qualified

-- ============================================================================
-- Step 4: Replace with single-round permutation (24 iterations)
-- ============================================================================

-- | Step 4: State machine with 24 single-round iterations
-- Uses keccakF1600Round instead of full keccakF1600
-- data State4
--   = Absorb
--   | Permute (Index 24) (Vec 1600 Bit)
--   -- | Squeeze (Vec 1600 Bit)
--   deriving (Show, Eq, Generic, NFDataX)

-- (Index 26, Vec 1600 Bit) -- (roundCount, currentState)
-- roundCount: 0 = initial, 1-24 = permutation rounds, 25 = done
type State4 = (Index 26, BitVector 1600) -- (roundCount, currentState)

stateful4 ::
  forall dom digest msgBits n.
  ( HiddenClockResetEnable dom,
    KnownNat digest,
    KnownNat msgBits,
    KnownNat n,
    digest <= 1088,
    n ~ Sponge.Stateful.PaddedBlocks 1088 msgBits,
    msgBits + 2 <= n * 1088,
    msgBits + 4 <= n * 1088
  ) =>
  (Signal dom (Index 24, BitVector 1600) -> Signal dom (BitVector 1600)) -> -- Permutation component
  Signal dom (BitVector msgBits) ->
  Signal dom (BitVector digest)
stateful4 permutationComponent msgSig = digestSig
  where
    -- FSM with feedback loop through permutation component
    (digestSig, permIn) = unbundle $ mealy step (0, 0) (bundle (fmap pack msgSig, stateAfterPerm))

    -- Permutation component instantiation
    stateAfterPerm = permutationComponent permIn

    step :: State4 -> (BitVector msgBits, BitVector 1600) -> (State4, (BitVector digest, (Index 24, BitVector 1600)))
    step (cnt, state) (msg, permResult)
      | cnt == 0 =
          -- Absorb message (no permutation yet)
          let -- Use foldl to absorb blocks, but XOR only (no permutation in absorption)
              absorb :: Vec n (Vec 1088 Bit) -> Vec 1600 Bit
              absorb = foldl absorbBlock (repeat 0)
              absorbBlock :: Vec 1600 Bit -> Vec 1088 Bit -> Vec 1600 Bit
              absorbBlock s block = zipWith xor s (block ++ repeat @512 0)

              blocks = pack (absorb (Hash.Combinational.pad @msgBits @n (unpack msg)))
           in ((1, blocks), (0, (0, blocks))) -- Move to round 1
      | cnt >= 1 && cnt <= 24 =
          -- Run one round of permutation
          let roundIdx :: Index 24
              roundIdx = resize (cnt - 1) -- Round indices are 0-23, resize from Index 26 to Index 24
              stateAsBitVector = pack state :: BitVector 1600
           in ((cnt + 1, unpack permResult), (0, (roundIdx, stateAsBitVector))) -- Continue to next round
      | otherwise =
          -- Done (cnt == 25): extract digest and reset
          let rateBlock = leToPlusKN @1088 @1600 takeI (unpack state) :: Vec 1088 Bit
              digest = leToPlusKN @digest @1088 takeI rateBlock :: Vec digest Bit
           in ((0, 0), (pack digest, (0, 0))) -- Reset for next message

--------------------------------------------------------------------------------
-- Stateful4 Sponge: Single-round permutation with 24 iterations
--------------------------------------------------------------------------------

-- | Stateful4 sponge construction - parameterized by permutation component
-- Similar to Sponge.spongeAxi, but for the simpler stateful4 interface
stateful4Sponge ::
  forall dom digest msgBits n.
  ( HiddenClockResetEnable dom,
    KnownNat digest,
    KnownNat msgBits,
    KnownNat n,
    digest <= 1088,
    n ~ DivRU (msgBits + 2) 1088,
    msgBits + 2 <= n * 1088,
    msgBits + 4 <= n * 1088
  ) =>
  (Signal dom (Index 24, BitVector 1600) -> Signal dom (BitVector 1600)) -> -- Permutation component
  Signal dom (BitVector msgBits) -> -- Input message
  Signal dom (BitVector digest) -- Output digest
stateful4Sponge = stateful4 @dom @digest @msgBits
