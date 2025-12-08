{-# LANGUAGE TypeApplications #-}

module Hash.Stateful6
  ( -- * Stateful6 SHA3 Top Entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Permutation.KeccakF1600 as Perm
import qualified Sponge.Stateful6

--------------------------------------------------------------------------------
-- Stateful6 SHA3-256 Top Entity (24 single-round iterations)
--------------------------------------------------------------------------------

type MsgBits = 64
type DigestBits = 64

-- Wrapper function for module naming control
-- OPAQUE ensures module boundary; function name determines module name
{-# OPAQUE spongeFSM #-}
spongeFSM :: Index 24 -> BitVector 1600 -> BitVector 1600
spongeFSM = Perm.keccakF1600Round

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Stateful6_SHA3",
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
    Sponge.Stateful6.sponge @System spongeFSM msgSig
