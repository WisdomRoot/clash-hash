module Permutation.Rho
  ( topEntity,
    rhoReversed,
  )
where

import Clash.Prelude
import Permutation qualified

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Rho",
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
topEntity _clk _rst _en = fmap (pack . Permutation.rhoF1600 . unpack)

{-# ANN
  rhoReversed
  ( Synthesize
      { t_name = "RhoRev",
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
{-# NOINLINE rhoReversed #-}
rhoReversed ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 1600) ->
  Signal System (BitVector 1600)
rhoReversed _clk _rst _en = fmap (pack . Permutation.rhoF1600Reversed . unpack)
