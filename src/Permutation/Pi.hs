module Permutation.Pi
  ( topEntity,
    piReversed,
  )
where

import Clash.Prelude
import Permutation qualified

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
topEntity _clk _rst _en = fmap (pack . Permutation.piF1600 . unpack)

{-# ANN
  piReversed
  ( Synthesize
      { t_name = "PiRev",
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
{-# NOINLINE piReversed #-}
piReversed ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 1600) ->
  Signal System (BitVector 1600)
piReversed _clk _rst _en = fmap (pack . Permutation.piF1600Reversed . unpack)
