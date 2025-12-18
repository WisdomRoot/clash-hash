{-# LANGUAGE TypeApplications #-}

module Permutation.WithoutTheta0
  ( rhoF1600,
    piF1600,
    chiF1600,
    iotaF1600,
    keccakF1600Round,
    keccakF1600,
    topEntity,
  )
where

import Clash.Prelude
import Permutation.Constants qualified as Constants

--------------------------------------------------------------------------------
-- Round primitives (theta removed)
--------------------------------------------------------------------------------

rev :: Index 1600 -> Index 1600
rev i = 1599 - i

chiF1600 :: Vec 1600 Bit -> Vec 1600 Bit
chiF1600 bv = map (\(i0, i1, i2) -> bv ! rev i0 `xor` (complement (bv ! rev i1) .&. bv ! rev i2)) $(Constants.chiReversed 6)

piF1600 :: Vec 1600 Bit -> Vec 1600 Bit
piF1600 bv = map ((bv !) . rev) Constants.pi6Reversed

rhoF1600 :: Vec 1600 Bit -> Vec 1600 Bit
rhoF1600 bv = map (bv !) Constants.rho6

iotaF1600 :: Index 24 -> Vec 1600 Bit -> Vec 1600 Bit
iotaF1600 roundIdx v =
  let lanes :: Vec 25 (Vec 64 Bit)
      lanes = unconcatI v
      rc :: Vec 64 Bit
      rc = $(Constants.iota) !! roundIdx
      lane0' = zipWith xor (head lanes) rc
   in concat (lane0' :> tail lanes)

--------------------------------------------------------------------------------
-- Permutation without theta
--------------------------------------------------------------------------------

{-# OPAQUE keccakF1600Round #-}
keccakF1600Round :: Index 24 -> BitVector 1600 -> BitVector 1600
keccakF1600Round roundIdx = pack . iotaF1600 roundIdx . chiF1600 . piF1600 . rhoF1600 . unpack

keccakF1600 :: BitVector 1600 -> BitVector 1600
keccakF1600 initialState =
  foldl applyRound initialState (indicesI @24)
  where
    applyRound state roundIdx = keccakF1600Round roundIdx state

--------------------------------------------------------------------------------
-- Top entity
--------------------------------------------------------------------------------

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF1600_WithoutTheta0",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "ROUND_IDX",
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
  Signal System (Index 24, BitVector 1600) ->
  Signal System (BitVector 1600)
topEntity _clk _rst _en = fmap (uncurry keccakF1600Round)
