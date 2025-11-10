{-# LANGUAGE TypeApplications #-}

module KeccakF200
  ( thetaF200,
    rhoF200,
    piF200,
    chiF200,
    iotaF200,
    topEntity,
  )
where

import Clash.Prelude
import qualified Constants
import Sponge (SpongeParameter, sponge)

-- Theta transformation: XOR with column parities
thetaF200 :: BitVector 200 -> BitVector 200
thetaF200 bv =
  ifoldl
    ( \acc idx indices11 ->
        let bitOut = fold xor (map (bv !) indices11)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.theta 3)

-- Chi transformation expressed directly on BitVector
chiF200 :: BitVector 200 -> BitVector 200
chiF200 bv =
  ifoldl
    ( \acc idx (i0, i1, i2) ->
        let bitOut = bv ! i0 `xor` (complement (bv ! i1) .&. bv ! i2)
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.chi 3)

-- Pi transformation: bit permutation on BitVector
piF200 :: BitVector 200 -> BitVector 200
piF200 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.pi 3)

-- Rho transformation: bit permutation on BitVector (lane rotation)
rhoF200 :: BitVector 200 -> BitVector 200
rhoF200 bv =
  ifoldl
    ( \acc idx srcIdx ->
        let bitOut = bv ! srcIdx
         in replaceBit idx bitOut acc
    )
    0
    $(Constants.rho 3)

iotaF200 :: Index 24 -> BitVector 200 -> BitVector 200
iotaF200 roundIdx bv =
  let lane0 = slice d7 d0 bv -- Extract first 8 bits (lane 0)
      lane0' = lane0 `xor` truncateB ($(Constants.iota) !! roundIdx) -- XOR with selected round constant
   in slice d199 d8 bv ++# lane0' -- Replace bits 0-7 with result

-- Complete Keccak-f[200] round: Theta, Rho, Pi, Chi, Iota
keccakF200Round :: Index 24 -> BitVector 200 -> BitVector 200
keccakF200Round roundIdx =
  iotaF200 roundIdx . chiF200 . piF200 . rhoF200 . thetaF200

-- | Full Keccak-f[200] permutation: 18 rounds (12 + 2*l where l=3)
-- Applies all rounds in sequence using the round constants
keccakF200 :: BitVector 200 -> BitVector 200
keccakF200 initialState =
  foldl applyRound initialState (indicesI @18)
  where
    applyRound state roundIdx = keccakF200Round (resize roundIdx) state

-- | Keccak-f[200] sponge function with configurable rate/capacity.
-- Instantiation: r=144, capacity=56, message=140 bits, output=128 bits
-- n = (140 + 144 + 1) / 144 = 1 absorb block
-- k = 128 / 144 = 0 squeeze blocks (128 < 144, so 1 total block suffices)
keccakF200Sponge :: forall r n m k d. (SpongeParameter 200 r n m k d) => BitVector m -> BitVector d
keccakF200Sponge = sponge @200 @r @n @m @k @d keccakF200

-- Concrete parameters for synthesis
type Rate = 144
type MessageBits = 140
type OutputBits = 128
type NumAbsorbBlocks = 1
type NumSqueezeBlocks = 0

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "KeccakF200_Sponge",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "DIN"
          ],
        t_output = PortName "DOUT"
      }
  )
  #-}
{-# OPAQUE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector MessageBits) ->
  Signal System (BitVector OutputBits)
topEntity = exposeClockResetEnable $ fmap spongeStep
  where
    spongeStep :: BitVector MessageBits -> BitVector OutputBits
    spongeStep = keccakF200Sponge @Rate @NumAbsorbBlocks @MessageBits @NumSqueezeBlocks @OutputBits
