{-# LANGUAGE TypeApplications #-}

module Sponge.Stateful4
  ( -- * Stateful4 Sponge
    sponge,
  )
where

import Clash.Prelude hiding (permute)
import Hash.Combinational qualified

data State
  = Absorb
  | Permute (Index 24) (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

-- | Stateful sponge, no streaming interface, fixed 1084-bit input / 256-bit output
{-# OPAQUE sponge #-}
sponge ::
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
sponge permute = mealy step Absorb
  where
    step :: State -> BitVector msgBits -> (State, BitVector digest)
    step Absorb msg =
      -- Absorb message (no permutation yet)
      let absorb :: Vec 1600 Bit -> Vec 1088 Bit -> Vec 1600 Bit
          absorb s block = zipWith xor s (block ++ repeat @512 0)
          blocks = pack (foldl absorb (repeat 0) (Hash.Combinational.pad @msgBits @n (unpack msg)))
       in (Permute 0 blocks, 0)
    step (Permute roundCount state) _msg =
      -- Run one round of permutation INSIDE step
      let permuted = state
        -- permute roundCount state
          rateBlock = leToPlusKN @1088 @1600 takeI (unpack permuted) :: Vec 1088 Bit
          digest = leToPlusKN @digest @1088 takeI rateBlock :: Vec digest Bit
       in (Permute (roundCount + 1) permuted, pack digest)
