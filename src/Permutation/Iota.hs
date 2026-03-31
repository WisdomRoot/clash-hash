module Permutation.Iota
  ( topEntity,
    iotaReversed,
  )
where

import Clash.Prelude
import Permutation qualified

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
topEntity _clk _rst _en = fmap (uncurry Permutation.iotaF1600)

{-# ANN
  iotaReversed
  ( Synthesize
      { t_name = "IotaRev",
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
{-# NOINLINE iotaReversed #-}
iotaReversed ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Index 24, BitVector 1600) ->
  Signal System (BitVector 1600)
iotaReversed _clk _rst _en = fmap (uncurry Permutation.iotaF1600Reversed)
