module Permutation.Pi
  ( topEntity,
  )
where

import Clash.Prelude
import Permutation.Perm qualified as Perm

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Pi",
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
topEntity _clk _rst _en = fmap (pack . Perm.piF1600Reversed . unpack)
