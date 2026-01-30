{-# LANGUAGE TypeApplications #-}

module Hash.NonPipelined.SHA3512
  ( -- * NonPipelined SHA3-512 Top Entity
    topEntity,
  )
where

import AXI4Stream (AXI4Stream)
import Clash.Prelude
import Permutation qualified
import Sponge.NonPipelined.SHA3512 qualified

--------------------------------------------------------------------------------
-- NonPipelined SHA3-512 Top Entity
--------------------------------------------------------------------------------

type MsgBits = 64

type DigestBits = 64

-- Wrapper function for module naming control
-- OPAQUE ensures module boundary; function name determines module name
{-# OPAQUE spongeFSM #-}
spongeFSM :: Index 24 -> BitVector 1600 -> BitVector 1600
spongeFSM = Permutation.keccakF1600Reversed

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "SHA3_512_NonPipelined",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "TREADY",
            PortProduct
              "MSG"
              [ PortName "MSG_TDATA",
                PortName "MSG_TVALID",
                PortName "MSG_TLAST",
                PortName "MSG_FLUSH"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortName "DIGEST_TDATA",
              PortName "DIGEST_TVALID",
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
  Signal System Bool -> -- output tready
  Signal System (AXI4Stream MsgBits, Bool) -> -- Input message with flush signal
  Signal System (AXI4Stream DigestBits, Bool) -- Output digest (AXI4-Stream), input tready
topEntity clk rst en treadySig inputSig =
  withClockResetEnable clk rst en
    $ let (msgSig, flushSig) = unbundle inputSig
       in Sponge.NonPipelined.SHA3512.sponge @System spongeFSM (bundle (msgSig, treadySig, flushSig))
