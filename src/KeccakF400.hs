{-# LANGUAGE TypeApplications #-}

module KeccakF400
  ( -- * SHA3 Top Entity
    topEntity,
  )
where

import Clash.Prelude
import KeccakF400.Permutation hiding (topEntity)
import qualified Sponge

--------------------------------------------------------------------------------
-- SHA3-f[400] AXI4-Stream Top Entity
--------------------------------------------------------------------------------

type Rate = 64

type DigestBits = 128

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF400_SHA3",
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
{-# OPAQUE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool -> -- S_AXIS_TVALID
  Signal System (BitVector Rate) -> -- S_AXIS_TDATA
  Signal System Bool -> -- S_AXIS_TLAST
  Signal System Bool -> -- M_AXIS_TREADY
  ( Signal System Bool, -- S_AXIS_TREADY
    Signal System Bool, -- M_AXIS_TVALID
    Signal System (BitVector Rate), -- M_AXIS_TDATA
    Signal System Bool -- M_AXIS_TLAST
  )
topEntity clk rst en sAxisTValid sAxisTData sAxisTLast mAxisTReady =
  withClockResetEnable clk rst en
    $ Sponge.spongeAxi @System @400 @Rate @DigestBits @20
      padBlock
      (keccakF400Round . resize)
      sAxisTValid
      sAxisTData
      sAxisTLast
      mAxisTReady
  where
    -- SHA3 pad10*1 block: 0b01 || 1 || 0...0 || 1
    -- For Rate=64: LSB [01 1 00...00 1] MSB (64 bits total)
    padBlock :: BitVector Rate
    padBlock = (1 :: BitVector 1) ++# (0 :: BitVector 60) ++# (0b110 :: BitVector 3)
