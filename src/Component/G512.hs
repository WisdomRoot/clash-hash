{-# LANGUAGE TypeApplications #-}

module Component.G512
  ( topEntity,
  )
where

import AXI4Stream (AXI4Stream)
import Clash.Prelude
import Parameter
import Permutation qualified
import Sponge.NonPipelined.SHA3512N256 qualified

--------------------------------------------------------------------------------
-- Component G512 (MK-KEM-512 / 256-bit input / 256-bit output)
--------------------------------------------------------------------------------

-- Wrapper function for module naming control
-- OPAQUE ensures module boundary; function name determines module name
{-# OPAQUE spongeFSM #-}
spongeFSM :: Index 24 -> BitVector 1600 -> BitVector 1600
spongeFSM = Permutation.keccakF1600

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Component_G_512_I256_O256",
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
  Signal System (AXI4Stream 256, Bool) -> -- Input message with flush signal
  Signal System (AXI4Stream 256, Bool) -- Output digest (AXI4-Stream), input tready
topEntity clk rst en treadySig inputSig =
  withClockResetEnable clk rst en
    $ let (msgSig, flushSig) = unbundle inputSig
       in Sponge.NonPipelined.SHA3512N256.sponge @System MLKEM512 spongeFSM (bundle (msgSig, treadySig, flushSig))
