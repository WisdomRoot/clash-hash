module Permutation.Iota
  ( topEntity,
  )
where

import Clash.Prelude
import Permutation.Perm qualified as Perm

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Iota",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [PortName "ROUND_IDX", PortName "STATE_IN"]
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
  Signal System (Index 24, BitVector 1600) ->
  Signal System (BitVector 1600)
topEntity _clk _rst _en = fmap (uncurry Perm.iotaF1600Reversed)
