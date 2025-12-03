{-# LANGUAGE TypeApplications #-}

module Hash.Stateful4
  ( -- * Stateful4 SHA3 Top Entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Permutation.KeccakF1600 as Perm
import qualified Sponge.Stateful4

--------------------------------------------------------------------------------
-- Stateful4 SHA3-256 Top Entity (24 single-round iterations)
--------------------------------------------------------------------------------

-- For "abc" test: 24-bit message, 256-bit digest (SHA3-256)
type MsgBits = 1088
type DigestBits = 256

-- Both OPAQUE pragmas work together:
-- 1. keccakF1600Round OPAQUE creates a reusable permutation module
-- 2. permutationFn OPAQUE forces module boundary at call site
--    (without it, keccakF1600Round gets inlined when passed as parameter)
{-# OPAQUE permutationFn #-}
permutationFn :: Index 24 -> BitVector 1600 -> BitVector 1600
permutationFn = Perm.keccakF1600Round

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Stateful4_SHA3",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "MSG"
          ],
        t_output = PortName "DIGEST"
      }
  )
  #-}
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector MsgBits) -> -- Input message
  Signal System (BitVector DigestBits) -- Output digest (256 bits for SHA3-256)
topEntity clk rst en msgSig =
  withClockResetEnable clk rst en $
    Sponge.Stateful4.stateful4Sponge @System @DigestBits @MsgBits permutationFn msgSig
