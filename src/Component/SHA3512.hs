module Component.SHA3512
  ( i64o64,
    i64o64Core,
  )
where

import AXI4Stream
import Clash.Prelude
import Permutation qualified
import Sponge.NonPipelined.SHA3512N qualified as SHA3512N

type InputBits = 64
type OutputBits = 64

{-# OPAQUE spongeFSM #-}
spongeFSM :: Index 24 -> BitVector 1600 -> BitVector 1600
spongeFSM = Permutation.keccakF1600

i64o64Core ::
  PipeCtrl2 dom Bool InputBits OutputBits
i64o64Core =
  PipeCtrl2Net $ \outputReady flushSig inputStream ->
    let inputWithFlush = bundle (inputStream, outputReady, flushSig)
        (outputStream, inputReady) = unbundle (SHA3512N.sponge spongeFSM inputWithFlush)
     in (inputReady, outputStream)

{-# ANN
  i64o64
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "SHA3_TREADY",
            PortProduct
              "MSG"
              [ PortName "TDATA",
                PortName "TVALID",
                PortName "TLAST",
                PortName "FLUSH"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "SHA3" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i64o64 #-}
i64o64 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream InputBits, Bool) ->
  Signal System (AXI4Stream OutputBits, Bool)
i64o64 = toDUTCtrl2 i64o64Core
