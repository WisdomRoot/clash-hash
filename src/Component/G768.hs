module Component.G768
  ( i256o256,
    i256o256Stream,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.G.Common qualified as Common
import Parameter (MLKEM (MLKEM768))

absorb32k3 :: BitVector 256 -> BitVector 1600
absorb32k3 = Common.absorb32WithMLKEM MLKEM768

i256o256Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 256 256
i256o256Core = Common.core absorb32k3

{-# NOINLINE i256o256Stream #-}
i256o256Stream ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream 256, Bool) ->
  Signal System (AXI4Stream 256, Bool)
i256o256Stream clk rst en treadySig inputSig =
  withClockResetEnable clk rst en $
    let (msgSig, _flushSig) = unbundle inputSig
        (inReadySig, outStreamSig) = Common.core absorb32k3 (treadySig, msgSig)
     in bundle (outStreamSig, inReadySig)

{-# ANN
  i256o256
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
{-# NOINLINE i256o256 #-}
i256o256 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 256, Bool) ->
  Signal System (AXI4Stream 256, Bool)
i256o256 = toDUT i256o256Core
