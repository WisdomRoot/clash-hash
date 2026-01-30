module Component.Dev
  ( topEntity,
  )
where

import Clash.Prelude
import Sponge.XOR qualified as XOR

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Component_Dev",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortName "STATE",
                PortName "BLOCK",
                PortName "BEAT"
              ]
          ],
        t_output = PortName "STATE_OUT"
      }
  )
  #-}
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 1600, BitVector 64, Index 17) ->
  Signal System (BitVector 1600)
topEntity clk rst en =
  withClockResetEnable clk rst en (fmap applyXor)
  where
    applyXor (state, block, beat) = XOR.staticXOR256' state block beat
