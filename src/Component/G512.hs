{-# LANGUAGE TypeApplications #-}

module Component.G512
  ( i256o256,
    i256o256Stream,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.G.Common qualified as Common
import Parameter
import Permutation qualified

{-# OPAQUE spongeFSM #-}
spongeFSM :: Index 24 -> BitVector 1600 -> BitVector 1600
spongeFSM = Permutation.keccakF1600

i256o256Core ::
  HiddenClockResetEnable dom =>
  Pipe2 dom 256 256
i256o256Core inputSig =
  let (msgSig, treadySig) = unbundle inputSig
   in i256o256Hidden treadySig (bundle (msgSig, pure False))

{-# NOINLINE i256o256Hidden #-}
i256o256Hidden ::
  HiddenClockResetEnable dom =>
  Signal dom Bool ->
  Signal dom (AXI4Stream 256, Bool) ->
  Signal dom (AXI4Stream 256, Bool)
i256o256Hidden treadySig inputSig =
  let (msgSig, flushSig) = unbundle inputSig
   in Common.sponge MLKEM512 spongeFSM (bundle (msgSig, treadySig, flushSig))

{-# NOINLINE i256o256Stream #-}
i256o256Stream ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System (AXI4Stream 256, Bool) ->
  Signal System (AXI4Stream 256, Bool)
i256o256Stream clk rst en treadySig inputSig =
  withClockResetEnable clk rst en
    $ i256o256Hidden treadySig inputSig

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
i256o256 = toDUT2 i256o256Core
