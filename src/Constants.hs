{-# LANGUAGE TemplateHaskell #-}

module Constants
  ( iota
  ) where

import Clash.Prelude
import qualified Constants.TH as TH

-- | All 24 round constants for Keccak-f, 64 bits each.
--
-- The constants are generated at compile time via Template Haskell so that
-- synthesized hardware sees a literal Vec instead of rebuilding the LFSR.
iota :: Vec 24 (BitVector 64)
iota = $(TH.iotaTH)
