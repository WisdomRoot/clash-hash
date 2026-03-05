module Component.G
  ( i274o256,
    i274o256Stream,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.G.Common qualified as Common
import Sponge.NonPipelinedN256 (complementAt)

absorb33 :: BitVector 274 -> BitVector 1600
absorb33 msg274 =
  let msg264 = slice (SNat @263) (SNat @0) msg274
      placed = (0 :: BitVector 1336) ++# msg264
   in complementAt 575 . complementAt 266 . complementAt 265 $ placed

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
i274o256Core = Common.core absorb33

{-# NOINLINE i274o256Stream #-}
i274o256Stream ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream 274, Bool) ->
  Signal System (AXI4Stream 256, Bool)
i274o256Stream clk rst en treadySig inputSig =
  withClockResetEnable clk rst en $
    let (msgSig, _flushSig) = unbundle inputSig
        (inReadySig, outStreamSig) = Common.core absorb33 (treadySig, msgSig)
     in bundle (outStreamSig, inReadySig)
