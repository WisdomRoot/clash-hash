{-# LANGUAGE TemplateHaskell #-}

module KeccakF100
  ( keccakf100
  , topEntity
  ) where

import Clash.Prelude hiding (pi)
import SHA3internal

-- Keccak-f[100]: l=2, w=4, b=100, 16 rounds
-- Using lane-based primitives to avoid constant table generation issues

type State100 = State 100

-- | Pre-computed round constants for w=4
roundConstants4 :: Vec 16 (Vec 4 Bit)
roundConstants4 = roundConstantsLane @2 @4

-- | Keccak-f[100] - full 16 rounds using lane-based primitives
-- w = 4 bits, l = 2, rounds = 12 + 2*2 = 16
keccakf100 :: State100 -> State100
keccakf100 = lanesToState . applyRounds roundConstants4 . stateToLanes

-- Hardware top entity - one round of Keccak-f[100]
{-# ANN topEntity
  (Synthesize
    { t_name = "Keccakf100_OneRound"
    , t_inputs = [ PortName "CLK"
                 , PortName "RST"
                 , PortName "EN"
                 , PortName "DIN"
                 ]
    , t_output = PortName "DOUT"
    }) #-}
{-# OPAQUE topEntity #-}

topEntity :: Clock System
          -> Reset System
          -> Enable System
          -> Signal System State100
          -> Signal System State100
topEntity = exposeClockResetEnable $ fmap oneRound
  where
    oneRound = lanesToState . laneRound (head roundConstants4) . stateToLanes
