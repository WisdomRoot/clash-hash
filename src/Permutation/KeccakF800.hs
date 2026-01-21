{-# LANGUAGE TypeApplications #-}

module Permutation.KeccakF800
  ( -- * Round primitives
    thetaF800,
    rhoF800,
    piF800,
    chiF800,
    iotaF800,
    -- * Permutation
    keccakF800Round,
    keccakF800,
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
thetaF800 :: Vec 800 Bit -> Vec 800 Bit
thetaF800 bv = bitCoerce $ map (fold xor . map (bv !)) $(Constants.theta 5)

-- Chi transformation
chiF800 :: Vec 800 Bit -> Vec 800 Bit
chiF800 bv = bitCoerce $ map (\(i0, i1, i2) -> bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)) $(Constants.chi 5)

-- Pi transformation: bit permutation
piF800 :: Vec 800 Bit -> Vec 800 Bit
piF800 bv = map (bv !) $(Constants.pi 5)

-- Rho transformation: bit permutation (lane rotation)
rhoF800 :: Vec 800 Bit -> Vec 800 Bit
rhoF800 bv = bitCoerce $ map (bv !) $(Constants.rho 5)

-- Iota transformation: XOR lane 0 with round constant
iotaF800 :: Index 24 -> Vec 800 Bit -> Vec 800 Bit
iotaF800 roundIdx v =
  let lane0   :: Vec 32 Bit
      lane0   = take d32 v
      rc      :: Vec 32 Bit
      rc      = takeI ($(Constants.iota) !! roundIdx)  -- Take lower 32 bits from 64-bit constant
      lane0'  = zipWith xor lane0 rc
   in lane0' ++ drop d32 v

--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

-- Complete Keccak-f[800] round: Theta, Rho, Pi, Chi, Iota
keccakF800Round :: Index 24 -> BitVector 800 -> BitVector 800
keccakF800Round roundIdx =
  pack . iotaF800 roundIdx . chiF800 . piF800 . rhoF800 . thetaF800 . unpack

keccakF800 :: BitVector 800 -> BitVector 800
keccakF800 initialState =
  foldl applyRound initialState (indicesI @22)
  where
    applyRound state roundIdx = keccakF800Round (resize roundIdx) state

--------------------------------------------------------------------------------
-- Top entity for hardware synthesis
--------------------------------------------------------------------------------

-- | Top entity for single Keccak-f[800] round
--
-- This module generates a standalone combinational block for a single round,
-- which can be instantiated by the FSM to execute one round per cycle.
--
-- = Ports
--
-- * ROUND_IDX - 5-bit round index (0-23, though only 0-21 are used)
-- * STATE_IN - 800-bit input state
-- * STATE_OUT - 800-bit output state after one round
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF800_Round",
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
  Signal System (Index 24, BitVector 800) ->
  Signal System (BitVector 800)
topEntity _clk _rst _en = fmap (uncurry keccakF800Round)
