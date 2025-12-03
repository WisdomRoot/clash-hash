{-# LANGUAGE TypeApplications #-}

module Sponge.Stateful5
  ( -- * Stateful5 Sponge
    sponge,
  )
where

import Clash.Prelude hiding (permute)
import Hash.Combinational qualified

type MsgBits = 64

data State
  = Absorb (Index 17)
  | Permute (Index 24) (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

-- | Stateful sponge, no streaming interface, fixed 1084-bit input / 256-bit output
{-# OPAQUE sponge #-}
sponge ::
  forall dom digest n.
  ( HiddenClockResetEnable dom,
    KnownNat digest,
    KnownNat n,
    digest <= 1088,
    n ~ DivRU (MsgBits + 2) 1088,
    MsgBits + 2 <= n * 1088,
    MsgBits + 4 <= n * 1088
  ) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (BitVector MsgBits) -> -- Input message
  Signal dom (BitVector digest) -- Output digest
sponge permute = mealy step (Absorb 0)
  where
    step :: State -> BitVector MsgBits -> (State, BitVector digest)
    step (Absorb counter) msg
      | counter < 16 =
          let absorb :: Vec 1600 Bit -> Vec 1088 Bit -> Vec 1600 Bit
              absorb s block = zipWith xor s (block ++ repeat @512 0)
              blocks = pack (foldl absorb (repeat 0) (Hash.Combinational.pad @MsgBits @n (unpack msg)))
           in (Permute 0 blocks, 0)
      | otherwise =
          let absorb :: Vec 1600 Bit -> Vec 1088 Bit -> Vec 1600 Bit
              absorb s block = zipWith xor s (block ++ repeat @512 0)
              blocks = pack (foldl absorb (repeat 0) (Hash.Combinational.pad @MsgBits @n (unpack msg)))
           in (Permute 0 blocks, 0)
    step (Permute roundCount state) _msg =
      -- Run one round of permutation INSIDE step
      let permuted = permute roundCount state
          rateBlock = leToPlusKN @1088 @1600 takeI (unpack permuted) :: Vec 1088 Bit
          digest = leToPlusKN @digest @1088 takeI rateBlock :: Vec digest Bit
       in (Permute (roundCount + 1) permuted, pack digest)
