{-# LANGUAGE TypeApplications #-}

module Component.SHA3256
  ( i64o64,
    i64o64Core,
  )
where

import AXI4Stream
import Clash.Prelude
import Permutation qualified
import Sponge.NonPipelined.SHA3256N qualified as SHA3256N

type InputBits = 64
type OutputBits = 64

{-# OPAQUE spongeFSM #-}
spongeFSM :: Index 24 -> BitVector 1600 -> BitVector 1600
spongeFSM = Permutation.keccakF1600

i64o64Core ::
  HiddenClockResetEnable dom =>
  Pipe dom InputBits OutputBits
i64o64Core (outputReady, inputStream) =
  let inputWithFlush = bundle (inputStream, outputReady, pure False)
      (outputStream, inputReady) = unbundle (SHA3256N.sponge spongeFSM inputWithFlush)
   in (inputReady, outputStream)

{-# ANN
  i64o64
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "MSG" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "SHA3_TREADY"
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
  Signal System (AXI4Stream InputBits, Bool) ->
  Signal System (AXI4Stream OutputBits, Bool)
i64o64 = toDUT i64o64Core
