module Permutation.Chi
  ( topEntity,
    chiReversed,
  )
where

import Clash.Prelude
import Permutation qualified

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Chi",
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
topEntity _clk _rst _en = fmap (pack . Permutation.chiF1600 . unpack)

{-# ANN
  chiReversed
  ( Synthesize
      { t_name = "ChiRev",
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
{-# NOINLINE chiReversed #-}
chiReversed ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 1600) ->
  Signal System (BitVector 1600)
chiReversed _clk _rst _en = fmap (pack . Permutation.chiF1600Reversed . unpack)
