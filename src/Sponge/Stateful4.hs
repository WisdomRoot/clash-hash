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
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (BitVector msgBits) ->
  Signal dom (BitVector digest)
stateful4 permutationFn msgSig = fmap fst $ mealy step (0, 0) msgSig
  where
    step :: State4 -> BitVector msgBits -> (State4, (BitVector digest, ()))
    step (cnt, state) msg
      | cnt == 0 =
          -- Absorb message (no permutation yet)
          let absorb :: Vec n (Vec 1088 Bit) -> Vec 1600 Bit
              absorb = foldl absorbBlock (repeat 0)
              absorbBlock :: Vec 1600 Bit -> Vec 1088 Bit -> Vec 1600 Bit
              absorbBlock s block = zipWith xor s (block ++ repeat @512 0)
              blocks = pack (absorb (Hash.Combinational.pad @msgBits @n (unpack msg)))
           in ((1, blocks), (0, ()))
      | cnt >= 1 && cnt <= 24 =
          -- Run one round of permutation INSIDE step
          let roundIdx :: Index 24
              roundIdx = resize (cnt - 1)
              permuted = permutationFn roundIdx state
           in ((cnt + 1, permuted), (0, ()))
      | otherwise =
          -- Done (cnt == 25): extract digest and reset
          let rateBlock = leToPlusKN @1088 @1600 takeI (unpack state) :: Vec 1088 Bit
              digest = leToPlusKN @digest @1088 takeI rateBlock :: Vec digest Bit
           in ((0, 0), (pack digest, ()))

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
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (BitVector msgBits) -> -- Input message
  Signal dom (BitVector digest) -- Output digest
stateful4Sponge = stateful4 @dom @digest @msgBits
