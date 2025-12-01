{-# LANGUAGE TypeApplications #-}

module Sponge.Stateful4
  ( -- * Stateful4 Top Entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Sponge.Stateful

--------------------------------------------------------------------------------
-- Stateful4 Top Entity: Single-round permutation with 24 iterations
--------------------------------------------------------------------------------

-- For "abc" test: 24-bit message, 256-bit digest (SHA3-256)
type MsgBits = 24
type DigestBits = 256

{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation component
  Signal System (BitVector MsgBits) -> -- Input message
  Signal System (BitVector DigestBits) -- Output digest (256 bits for SHA3-256)
topEntity clk rst en permutationComponent msgSig =
  withClockResetEnable clk rst en $
    fmap pack $
      Sponge.Stateful.stateful4 @System @DigestBits @MsgBits permutationComponent $
        fmap unpack msgSig
