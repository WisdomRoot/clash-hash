module Permutation.Theta
  ( i1600o1600Core,
    i1600o1600,
    topEntity,
    thetaReversed,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Permutation qualified

i1600o1600Core :: (HiddenClockResetEnable dom) => Pipe dom 1600 1600
i1600o1600Core (outReady, inStream) = (outReady, fmap applyTheta inStream)
  where
    applyTheta s = s {tdata = pack (Permutation.thetaF1600 (unpack (tdata s)))}

{-# ANN
  i1600o1600
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "STATE_IN" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "STATE_OUT_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "STATE_OUT" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "STATE_IN_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i1600o1600 #-}
i1600o1600 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 1600, Bool) ->
  Signal System (AXI4Stream 1600, Bool)
i1600o1600 = toDUT i1600o1600Core

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Theta",
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
topEntity _clk _rst _en = fmap (pack . Permutation.thetaF1600 . unpack)

{-# ANN
  thetaReversed
  ( Synthesize
      { t_name = "ThetaRev",
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
{-# NOINLINE thetaReversed #-}
thetaReversed ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 1600) ->
  Signal System (BitVector 1600)
thetaReversed _clk _rst _en = fmap (pack . Permutation.thetaF1600Reversed . unpack)
