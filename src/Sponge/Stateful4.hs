{-# LANGUAGE TypeApplications #-}

module Sponge.Stateful4
  ( -- * Stateful4 Sponge
    stateful4Sponge,
  )
where

import Clash.Prelude
import qualified Sponge.Stateful

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
stateful4Sponge permutationComponent msgSig =
  fmap pack $
    Sponge.Stateful.stateful4 @dom @digest @msgBits permutationComponent $
      fmap unpack msgSig
