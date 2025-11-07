{-# LANGUAGE TemplateHaskell #-}

module Constants
  ( iota
  , chi
  , pi
  , rho
  , theta
  ) where

import Clash.Prelude hiding (pi)
import qualified Constants.TH as TH

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

-- | Theta transformation index lookup table for Keccak-f[200].
--
-- Contains 200 lists of 11 indices each.
-- For each output bit, these 11 indices identify the input bits to XOR together.
-- Theta formula: A'[i,j,k] = A[i,j,k] ⊕ parity(column j-1, bit k) ⊕ parity(column j+1, bit k-1)
-- Generated at compile time via Template Haskell.
theta :: Vec 200 (Vec 11 (Index 200))
theta = $(TH.theta)
