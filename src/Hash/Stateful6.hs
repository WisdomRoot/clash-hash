{-# LANGUAGE TypeApplications #-}

module Hash.Stateful6
  ( -- * Stateful6 SHA3 Top Entity
    topEntity,
  )
where

import AXI4Stream (AXI4Stream)
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
            PortName "TREADY",
            PortName "MSG"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "DIGEST_TDATA",
              PortName "DIGEST_TVALID",
              PortName "DIGEST_TREADY",
              PortName "DIGEST_TLAST"
            ]
      }
  )
  #-}
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool -> -- tready input (backpressure)
  Signal System (BitVector MsgBits) -> -- Input message
  Signal System (AXI4Stream DigestBits) -- Output digest (AXI4-Stream)
topEntity clk rst en treadySig msgSig =
  withClockResetEnable clk rst en $
    Sponge.Stateful6.sponge @System spongeFSM treadySig msgSig
