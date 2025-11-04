{-# LANGUAGE TemplateHaskell #-}

module ChiF200
  ( chiF200
  , topEntity
  ) where

import Clash.Prelude
import SHA3internal (State, SHA3Constants, sha3_constants, chi)

-- Chi transformation for b=200: l=3, w=8
-- Uses constant-table-driven chi function

type State200 = State 200

-- | Pre-computed constants for Keccak-f[200]
chiConstants :: SHA3Constants 3 8 200
chiConstants = $(lift $ sha3_constants @3 @8 @200)

-- | Chi transformation only - no other Keccak steps
chiF200 :: State200 -> State200
chiF200 = chi chiConstants

-- Hardware top entity - Chi transformation on 200-bit state
{-# ANN topEntity
  (Synthesize
    { t_name = "ChiF200_OneRound"
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
          -> Signal System State200
          -> Signal System State200
topEntity = exposeClockResetEnable $ fmap chiF200
