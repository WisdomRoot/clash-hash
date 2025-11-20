{-# LANGUAGE TypeApplications #-}

module KeccakF1600.Permutation
  ( -- * Round primitives
    thetaF1600,
    rhoF1600,
    piF1600,
    chiF1600,
    iotaF1600,
    -- * Permutation
    keccakF1600Round,
    keccakF1600,
    -- * Top entity
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants

--------------------------------------------------------------------------------
-- Round primitives
--------------------------------------------------------------------------------

-- Theta transformation: XOR with column parities
thetaF1600 :: Vec 1600 Bit -> Vec 1600 Bit
thetaF1600 bv = bitCoerce $ map (fold xor . map (bv !)) $(Constants.theta 6)

-- Chi transformation
chiF1600 :: Vec 1600 Bit -> Vec 1600 Bit
chiF1600 bv = bitCoerce $ map (\(i0, i1, i2) -> bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)) $(Constants.chi 6)

-- Pi transformation: bit permutation
piF1600 :: Vec 1600 Bit -> Vec 1600 Bit
piF1600 bv = map (bv !) $(Constants.pi 6)

-- Rho transformation: bit permutation (lane rotation)
rhoF1600 :: Vec 1600 Bit -> Vec 1600 Bit
rhoF1600 bv = bitCoerce $ map (bv !) $(Constants.rho 6)

-- Iota transformation: XOR lane 0 with round constant
iotaF1600 :: Index 24 -> Vec 1600 Bit -> Vec 1600 Bit
iotaF1600 roundIdx v =
  let lane0   :: Vec 64 Bit
      lane0   = take d64 v
      rc      :: Vec 64 Bit
      rc      = bitCoerce (($(Constants.iota) !! roundIdx) :: BitVector 64)
      lane0'  = zipWith xor lane0 rc
   in lane0' ++ drop d64 v

--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

-- Complete Keccak-f[1600] round: Theta, Rho, Pi, Chi, Iota
keccakF1600Round :: Index 24 -> BitVector 1600 -> BitVector 1600
keccakF1600Round roundIdx =
  pack . iotaF1600 roundIdx . chiF1600 . piF1600 . rhoF1600 . thetaF1600 . unpack

keccakF1600 :: BitVector 1600 -> BitVector 1600
keccakF1600 initialState =
  foldl applyRound initialState (indicesI @24)
  where
    applyRound state roundIdx = keccakF1600Round roundIdx state

--------------------------------------------------------------------------------
-- Top entity for hardware synthesis
--------------------------------------------------------------------------------

-- | Top entity for single Keccak-f[1600] round
--
-- This module generates a standalone combinational block for a single round,
-- which can be instantiated by the FSM to execute one round per cycle.
--
-- = Ports
--
-- * ROUND_IDX - 5-bit round index (0-23)
-- * STATE_IN - 1600-bit input state
-- * STATE_OUT - 1600-bit output state after one round
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF1600_Round",
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
