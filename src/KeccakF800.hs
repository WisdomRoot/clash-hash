{-# LANGUAGE TypeApplications #-}

module KeccakF800
  ( -- * SHA3 Top Entity
    topEntity,
  )
where

import Clash.Prelude
import qualified KeccakF800.Permutation as Perm
import qualified Sponge

--------------------------------------------------------------------------------
-- SHA3-f[800] AXI4-Stream Top Entity
--------------------------------------------------------------------------------

type Rate = 64

type DigestBits = 64

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF800_SHA3",
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
    $ Sponge.spongeAxi @System @800 @Rate @DigestBits @22
      (0b01 :: BitVector 2)
      (permutationComponent clk rst en)
      sAxisTValid
      sAxisTData
      sAxisTLast
      mAxisTReady
  where
    -- Instantiate permutation as a proper HDL component using explicit clk/rst/en
    -- This ensures a stable module instance instead of inline expansion
    permutationComponent ::
      Clock System ->
      Reset System ->
      Enable System ->
      Signal System (Index 22, BitVector 800) ->
      Signal System (BitVector 800)
    permutationComponent clk' rst' en' input =
      Perm.topEntity clk' rst' en' resizedInput24
      where
        -- Resize Index 22 to Index 24 for the permutation topEntity
        resizedInput24 = fmap (\(roundIdx, state) -> (resize roundIdx :: Index 24, state)) input
