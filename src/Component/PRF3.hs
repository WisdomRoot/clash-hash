module Component.PRF3
  ( topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.PRF.Common

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Component_PRF3",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "MSG_33B",
            PortName "DIGEST_TREADY"
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
  Signal System (BitVector 264) ->
  Signal System Bool ->
  Signal System (AXI4Stream 64, Bool)
topEntity clk rst en msgSig treadySig =
  withClockResetEnable clk rst en (hash Eta3 msgSig treadySig)
