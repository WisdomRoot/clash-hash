module Permutation.Theta0
  ( topEntity,
  )
where

import Clash.Prelude
import Permutation.P2 qualified as P2

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Theta0",
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
topEntity _clk _rst _en = fmap (pack . P2.thetaF1600 . unpack)
