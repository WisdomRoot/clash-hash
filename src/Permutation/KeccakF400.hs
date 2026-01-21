{-# LANGUAGE TypeApplications #-}

module Permutation.KeccakF400
  ( -- * Round primitives
    thetaF400,
    rhoF400,
    piF400,
    chiF400,
    iotaF400,
    -- * Permutation
    keccakF400Round,
    keccakF400,
    -- * Top entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Permutation.Constants as Constants

--------------------------------------------------------------------------------
-- Round primitives
--------------------------------------------------------------------------------

-- Theta transformation: XOR with column parities
thetaF400 :: Vec 400 Bit -> Vec 400 Bit
thetaF400 bv = bitCoerce $ map (fold xor . map (bv !)) $(Constants.theta 4)

-- Chi transformation
chiF400 :: Vec 400 Bit -> Vec 400 Bit
chiF400 bv = bitCoerce $ map (\(i0, i1, i2) -> bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)) $(Constants.chiReversed 4)

-- Pi transformation: bit permutation
piF400 :: Vec 400 Bit -> Vec 400 Bit
piF400 bv = map (bv !) $(Constants.pi 4)

-- Rho transformation: bit permutation (lane rotation)
rhoF400 :: Vec 400 Bit -> Vec 400 Bit
rhoF400 bv = bitCoerce $ map (bv !) $(Constants.rho 4)

-- Iota transformation: XOR lane 0 with round constant
iotaF400 :: Index 24 -> Vec 400 Bit -> Vec 400 Bit
iotaF400 roundIdx v =
  let lane0   :: Vec 16 Bit
      lane0   = take d16 v
      rc      :: Vec 16 Bit
      rc      = takeI ($(Constants.iota) !! roundIdx)  -- Take lower 16 bits from 64-bit constant
      lane0'  = zipWith xor lane0 rc
   in lane0' ++ drop d16 v

--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

-- Complete Keccak-f[400] round: Theta, Rho, Pi, Chi, Iota
keccakF400Round :: Index 24 -> BitVector 400 -> BitVector 400
keccakF400Round roundIdx =
  pack . iotaF400 roundIdx . chiF400 . piF400 . rhoF400 . thetaF400 . unpack

keccakF400 :: BitVector 400 -> BitVector 400
keccakF400 initialState =
  foldl applyRound initialState (indicesI @20)
  where
    applyRound state roundIdx = keccakF400Round (resize roundIdx) state

--------------------------------------------------------------------------------
-- Top entity for hardware synthesis
--------------------------------------------------------------------------------

-- | Top entity for single Keccak-f[400] round
--
-- This module generates a standalone combinational block for a single round,
-- which can be instantiated by the FSM to execute one round per cycle.
--
-- = Ports
--
-- * ROUND_IDX - 5-bit round index (0-23, though only 0-19 are used)
-- * STATE_IN - 400-bit input state
-- * STATE_OUT - 400-bit output state after one round
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF400_Round",
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
  Signal System (Index 24, BitVector 400) ->
  Signal System (BitVector 400)
topEntity _clk _rst _en = fmap (uncurry keccakF400Round)
