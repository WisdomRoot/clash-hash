module Permutation.Theta1
  ( topEntity,
  )
where

import Clash.Prelude
import Permutation.P3 qualified as P3

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Theta1",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "STATE_IN"
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
  Signal System (BitVector 1600) ->
  Signal System (BitVector 1600)
topEntity _clk _rst _en = fmap (pack . P3.thetaF1600 . unpack)
