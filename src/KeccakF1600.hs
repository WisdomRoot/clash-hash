{-# LANGUAGE TypeApplications #-}

module KeccakF1600
  ( -- * SHA3 Top Entity
    topEntity,
  )
where

import Clash.Prelude
import qualified KeccakF1600.Permutation as Perm
import qualified Sponge

--------------------------------------------------------------------------------
-- SHA3-f[1600] AXI4-Stream Top Entity (SHA3-256)
--------------------------------------------------------------------------------

type BusWidth = 64

type Rate = 1088

type DigestBits = 256

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF1600_SHA3",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "S_AXIS_TVALID",
            PortName "S_AXIS_TDATA",
            PortName "S_AXIS_TLAST",
            PortName "M_AXIS_TREADY"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "S_AXIS_TREADY",
              PortName "M_AXIS_TVALID",
              PortName "M_AXIS_TDATA",
              PortName "M_AXIS_TLAST"
            ]
      }
  )
  #-}
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool -> -- S_AXIS_TVALID
  Signal System (BitVector BusWidth) -> -- S_AXIS_TDATA
  Signal System Bool -> -- S_AXIS_TLAST
  Signal System Bool -> -- M_AXIS_TREADY
  ( Signal System Bool, -- S_AXIS_TREADY
    Signal System Bool, -- M_AXIS_TVALID
    Signal System (BitVector BusWidth), -- M_AXIS_TDATA
    Signal System Bool -- M_AXIS_TLAST
  )
topEntity clk rst en sAxisTValid sAxisTData sAxisTLast mAxisTReady =
  withClockResetEnable clk rst en
    $ Sponge.spongeAxi @System @1600 @BusWidth @Rate @DigestBits @24
      (0b01 :: BitVector 2)
      (permutationComponent clk rst en)
      sAxisTValid
      sAxisTData
      sAxisTLast
      mAxisTReady
  where
    -- Instantiate permutation as a proper HDL component using exposeClockResetEnable
    -- This ensures a stable module instance instead of inline expansion
    permutationComponent ::
      Clock System ->
      Reset System ->
      Enable System ->
      Signal System (Index 24, BitVector 1600) ->
      Signal System (BitVector 1600)
    permutationComponent clk' rst' en' input =
      Perm.topEntity clk' rst' en' transformed
      where
        -- Force Clash to create a separate component by transforming the signal
        -- Even though Index 24 needs no resize, we transform to prevent inlining
        transformed = fmap (\(roundIdx, state) -> (roundIdx, state)) input
