{-# LANGUAGE TemplateHaskell #-}

module SHA3F25
  ( keccakf25OneRound2
  , keccakf25OneRound3
  , topEntity
  ) where

import Clash.Prelude hiding (pi)
import SHA3internal

-- Keccak-f[25]: 5×5×1 bits, l=0, w=1, b=25
-- This is the minimal Keccak variant for synthesis testing

type State25 = State 25

-- One round of Keccak-f[25] using SHA3internal transformations (old version with theta2)
keccakf25OneRound2 :: State25 -> State25
keccakf25OneRound2 = iota constants25 0 . chi constants25 . pi constants25 . rho constants25 . theta2

-- One round of Keccak-f[25] using lane representation with theta3
-- Converts to lanes once, applies theta3, converts back, then applies remaining steps
keccakf25OneRound3 :: State25 -> State25
keccakf25OneRound3 = iota constants25 0 . chi constants25 . pi constants25 . rho constants25 . lanesToState . theta3 . stateToLanes

-- Hardware top entity
{-# ANN topEntity
  (Synthesize
    { t_name = "Keccakf25_OneRound"
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
          -> Signal System State25
          -> Signal System State25
topEntity = exposeClockResetEnable $ fmap keccakf25OneRound3
