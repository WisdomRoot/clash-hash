module Component.G
  ( i274o256
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.G.Common qualified as Common

{-# ANN
  i274o256
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "MSG" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "DIGEST_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "DIGEST" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i274o256 #-}
i274o256 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 274, Bool) ->
  Signal System (AXI4Stream 256, Bool)
i274o256 = toDUT i274o256Core

i274o256Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 274 256
i274o256Core = Common.core (Common.absorb274WithMLKEM Nothing)
