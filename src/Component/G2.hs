module Component.G2
  ( i256o512
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.G.Common qualified as Common
import Parameter (MLKEM (MLKEM512))

i256o512Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 256 512
i256o512Core = Common.core512 (Common.absorb32WithMLKEM (Just MLKEM512))

{-# ANN
  i256o512
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
{-# NOINLINE i256o512 #-}
i256o512 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 256, Bool) ->
  Signal System (AXI4Stream 512, Bool)
i256o512 = toDUT i256o512Core
