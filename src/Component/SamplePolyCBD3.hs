module Component.SamplePolyCBD3
  ( i264o12,
    i264o12Core,
    i264o24,
    i264o24Core,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.SamplePolyCBD qualified as SamplePolyCBD

{-# ANN
  i264o12
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
{-# NOINLINE i264o12 #-}
i264o12 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 264, Bool) ->
  Signal System (AXI4Stream 12, Bool)
i264o12 = toDUT i264o12Core

{-# ANN
  i264o24
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
{-# NOINLINE i264o24 #-}
i264o24 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 264, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i264o24 = toDUT i264o24Core

prependEta ::
  HiddenClockResetEnable dom =>
  BitVector 8 ->
  Pipe dom 264 272
prependEta eta (outReady, inStream) =
  let outStream = fmap prepend inStream
   in (outReady, outStream)
  where
    prepend (AXI4Stream msg valid lastBeat) =
      AXI4Stream (eta ++# msg) valid lastBeat

i264o12Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 264 12
i264o12Core = prependEta 3 ~> SamplePolyCBD.i272o12Core

i264o24Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 264 24
i264o24Core = prependEta 3 ~> SamplePolyCBD.i272o24Core
