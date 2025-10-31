{-# LANGUAGE TemplateHaskell #-}

module SHA3F25
  ( keccakf25OneRound
  , topEntity
  ) where

import Clash.Prelude hiding (pi)
import SHA3internal

-- Keccak-f[25]: 5×5×1 bits, l=0, w=1, b=25
-- This is the minimal Keccak variant for synthesis testing

type State25 = State 25

-- One round of Keccak-f[25] using SHA3internal transformations
keccakf25OneRound :: State25 -> State25
keccakf25OneRound = iota constants25 0 . chi constants25 . pi constants25 . rho constants25 . theta constants25

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
topEntity = exposeClockResetEnable $ fmap keccakf25OneRound
