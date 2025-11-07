{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Constants
  ( iota,
    chi,
    pi,
    rho,
    theta,
  )
where

import Clash.Prelude hiding (pi)
import qualified Constants.TH as TH
import Data.Type.Equality ((:~:) (Refl))

-- | All 24 round constants for Keccak-f, 64 bits each.
--
-- The constants are generated at compile time via Template Haskell so that
-- synthesized hardware sees a literal Vec instead of rebuilding the LFSR.
iota :: Index 24 -> BitVector 8
iota idx = truncateB ($(TH.iota) !! idx)

-- | Chi transformation index lookup table for Keccak-f[200].
--
-- Contains 200 index triples (i0, i1, i2) where:
--   output[idx] = state[i0] ⊕ (¬state[i1] ∧ state[i2])
-- Generated at compile time via Template Haskell.
chi :: Vec 200 (Index 200, Index 200, Index 200)
chi = $(TH.chi)

-- | Pi transformation permutation table for Keccak-f[200].
--
-- Contains 200 source indices for the pi permutation.
-- Pi formula: (i, j, k) -> (j, 3*i + j, k)
-- Generated at compile time via Template Haskell.
pi :: Vec 200 (Index 200)
pi = $(TH.pi)

-- | Rho transformation permutation table for Keccak-f[200].
--
-- Contains 200 source indices for the rho permutation.
-- Rho rotates each lane by a different offset according to the Keccak specification.
-- Generated at compile time via Template Haskell.
rho :: Vec 200 (Index 200)
rho = $(TH.rho)

-- | Theta transformation index lookup table parameterized by lane size.
--
-- Given Keccak parameter l (lane size w = 2^l, state size b = 25w), produce the
-- 11-source index vectors needed for each bit of the Theta step.
theta ::
  forall l w b.
  (Parameter l w b) =>
  Vec b (Vec 11 (Index b))
theta
  | Just Refl <- sameNat (SNat @l) (SNat @0) = $(TH.theta 0)
  | Just Refl <- sameNat (SNat @l) (SNat @1) = $(TH.theta 1)
  | Just Refl <- sameNat (SNat @l) (SNat @2) = $(TH.theta 2)
  | Just Refl <- sameNat (SNat @l) (SNat @3) = $(TH.theta 3)
  | Just Refl <- sameNat (SNat @l) (SNat @4) = $(TH.theta 4)
  | Just Refl <- sameNat (SNat @l) (SNat @5) = $(TH.theta 5)
  | Just Refl <- sameNat (SNat @l) (SNat @6) = $(TH.theta 6)
  | otherwise = errorX "theta: unsupported lane parameter"

----

type Parameter l w b =
  ( KnownNat l,
    KnownNat w,
    KnownNat b,
    w ~ (2 ^ l),
    b ~ 25 * w,
    0 <= l,
    l <= 6
  )
