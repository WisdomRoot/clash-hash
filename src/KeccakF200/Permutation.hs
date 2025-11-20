{-# LANGUAGE TypeApplications #-}

module KeccakF200.Permutation
  ( -- * Round primitives
    thetaF200,
    rhoF200,
    piF200,
    chiF200,
    iotaF200,

    -- * Top entity (round only)
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants

-- Theta transformation: XOR with column parities
thetaF200 :: Vec 200 Bit -> Vec 200 Bit
thetaF200 bv = bitCoerce $ map (fold xor . map (bv !)) $(Constants.theta 3)

-- Chi transformation expressed directly on BitVector
chiF200 :: Vec 200 Bit -> Vec 200 Bit
chiF200 bv = bitCoerce $ map (\(i0, i1, i2) -> bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)) $(Constants.chi 3)

-- Pi transformation: bit permutation on BitVector
piF200 :: Vec 200 Bit -> Vec 200 Bit
piF200 bv = map (bv !) $(Constants.pi 3)

-- Rho transformation: bit permutation on BitVector (lane rotation)
rhoF200 :: Vec 200 Bit -> Vec 200 Bit
rhoF200 bv = bitCoerce $ map (bv !) $(Constants.rho 3)

-- Iota transformation: XOR lane 0 with round constant
iotaF200 :: Index 24 -> Vec 200 Bit -> Vec 200 Bit
iotaF200 roundIdx v =
  let lane0   :: Vec 8 Bit
      lane0   = take d8 v
      rc      :: Vec 8 Bit
      rc      = bitCoerce (truncateB ($(Constants.iota) !! roundIdx) :: BitVector 8)
      lane0'  = zipWith xor lane0 rc
   in lane0' ++ drop d8 v

-- Complete Keccak-f[200] round: Theta, Rho, Pi, Chi, Iota
keccakF200Round :: Index 24 -> BitVector 200 -> BitVector 200
keccakF200Round roundIdx =
  pack . iotaF200 roundIdx . chiF200 . piF200 . rhoF200 . thetaF200 . unpack

--------------------------------------------------------------------------------
-- Top entity for hardware synthesis
--------------------------------------------------------------------------------

-- | Top entity for single Keccak-f[200] round
--
-- This module generates a standalone combinational block for a single round,
-- which can be instantiated by the FSM to execute one round per cycle.
--
-- = Ports
--
-- * ROUND_IDX - 5-bit round index (0-23, though only 0-17 are used)
-- * STATE_IN - 200-bit input state
-- * STATE_OUT - 200-bit output state after one round
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF200_Round",
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
  Signal System (Index 24, BitVector 200) ->
  Signal System (BitVector 200)
topEntity _clk _rst _en = fmap (uncurry keccakF200Round)
