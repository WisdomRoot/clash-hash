{-# LANGUAGE TemplateHaskell #-}

module Constants
  ( iota
  , chi
  , pi
  ) where

import Clash.Prelude hiding (pi)
import qualified Constants.TH as TH

-- | All 24 round constants for Keccak-f, 64 bits each.
--
-- The constants are generated at compile time via Template Haskell so that
-- synthesized hardware sees a literal Vec instead of rebuilding the LFSR.
iota :: Vec 24 (BitVector 64)
iota = $(TH.iota)

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
