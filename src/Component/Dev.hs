{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Component.Dev
  ( topEntity,
  )
where

import Clash.Prelude
import TH (mkRead, mkWrite)

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Component_Dev",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortName "STATE",
                PortName "BLOCK",
                PortName "BEAT"
              ]
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
  Signal System (BitVector 1600, BitVector 64, Index 25) ->
  Signal System (BitVector 1600)
topEntity clk rst en =
  withClockResetEnable clk rst en (fmap applyXor)
  where
    applyXor (state, block, beat) = baseline state block beat

-- baseline: 3114.860
-- current:  

baseline :: BitVector 1600 -> BitVector 64 -> Index 25 -> BitVector 1600
baseline state block beatCounter =
  case beatCounter of
    0 -> setSlice (SNat @63) (SNat @0) (slice (SNat @63) (SNat @0) state `xor` block) state
    1 -> setSlice (SNat @127) (SNat @64) (slice (SNat @127) (SNat @64) state `xor` block) state
    2 -> setSlice (SNat @191) (SNat @128) (slice (SNat @191) (SNat @128) state `xor` block) state
    3 -> setSlice (SNat @255) (SNat @192) (slice (SNat @255) (SNat @192) state `xor` block) state
    4 -> setSlice (SNat @319) (SNat @256) (slice (SNat @319) (SNat @256) state `xor` block) state
    5 -> setSlice (SNat @383) (SNat @320) (slice (SNat @383) (SNat @320) state `xor` block) state
    6 -> setSlice (SNat @447) (SNat @384) (slice (SNat @447) (SNat @384) state `xor` block) state
    7 -> setSlice (SNat @511) (SNat @448) (slice (SNat @511) (SNat @448) state `xor` block) state
    8 -> setSlice (SNat @575) (SNat @512) (slice (SNat @575) (SNat @512) state `xor` block) state
    9 -> setSlice (SNat @639) (SNat @576) (slice (SNat @639) (SNat @576) state `xor` block) state
    10 -> setSlice (SNat @703) (SNat @640) (slice (SNat @703) (SNat @640) state `xor` block) state
    11 -> setSlice (SNat @767) (SNat @704) (slice (SNat @767) (SNat @704) state `xor` block) state
    12 -> setSlice (SNat @831) (SNat @768) (slice (SNat @831) (SNat @768) state `xor` block) state
    13 -> setSlice (SNat @895) (SNat @832) (slice (SNat @895) (SNat @832) state `xor` block) state
    14 -> setSlice (SNat @959) (SNat @896) (slice (SNat @959) (SNat @896) state `xor` block) state
    15 -> setSlice (SNat @1023) (SNat @960) (slice (SNat @1023) (SNat @960) state `xor` block) state
    16 -> setSlice (SNat @1087) (SNat @1024) (slice (SNat @1087) (SNat @1024) state `xor` block) state
    17 -> setSlice (SNat @1151) (SNat @1088) (slice (SNat @1151) (SNat @1088) state `xor` block) state
    18 -> setSlice (SNat @1215) (SNat @1152) (slice (SNat @1215) (SNat @1152) state `xor` block) state
    19 -> setSlice (SNat @1279) (SNat @1216) (slice (SNat @1279) (SNat @1216) state `xor` block) state
    20 -> setSlice (SNat @1343) (SNat @1280) (slice (SNat @1343) (SNat @1280) state `xor` block) state
    21 -> setSlice (SNat @1407) (SNat @1344) (slice (SNat @1407) (SNat @1344) state `xor` block) state
    22 -> setSlice (SNat @1471) (SNat @1408) (slice (SNat @1471) (SNat @1408) state `xor` block) state
    23 -> setSlice (SNat @1535) (SNat @1472) (slice (SNat @1535) (SNat @1472) state `xor` block) state
    _ -> setSlice (SNat @1599) (SNat @1536) (slice (SNat @1599) (SNat @1536) state `xor` block) state

$(mkRead "readLane" 1600 [(i, i * 64, 64) | i <- [0 .. 24]])
$(mkWrite "writeLane" 64 25)

new :: BitVector 1600 -> BitVector 64 -> Index 25 -> BitVector 1600
{-# INLINE new #-}
new state block index = writeLane (block `xor` readLane state index) index state
