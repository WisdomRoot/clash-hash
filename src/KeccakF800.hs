{-# LANGUAGE TemplateHaskell #-}

module KeccakF800
  ( keccakf800
  , topEntity
  ) where

import Clash.Prelude hiding (pi)
import SHA3internal

-- Keccak-f[800]: l=5, w=32, b=800, 22 rounds
-- This is a mid-sized Keccak variant for synthesis testing

type State800 = State 800

-- | Generic Keccak-p permutation
keccakp :: forall b nr l w. ( KeccakParameter l w b
                            , KnownNat nr
                            , 1 <= nr
                            , nr <= 12 + 2 * l
                            )
        => SHA3Constants l w b
        -> State b
        -> State b
keccakp c = foldl (flip f) id $ dropI @(12 + 2 * l - nr) indicesI where
  f i = (.) ( iota  c i
            . chi   c
            . pi    c
            . rho   c
            . theta c
            )

-- | Keccak-f[800] - full 22 rounds
-- w = 32 bits, l = 5, rounds = 12 + 2*5 = 22
keccakf800 :: State800 -> State800
keccakf800 = keccakp @800 @22 @5 @32 c where
  c = $(lift $ sha3_constants @5 @32 @800)

-- Hardware top entity - one round of Keccak-f[800]
{-# ANN topEntity
  (Synthesize
    { t_name = "Keccakf800_OneRound"
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
          -> Signal System State800
          -> Signal System State800
topEntity = exposeClockResetEnable $ fmap (iota c 0 . chi c . pi c . rho c . theta c) where
  c = $(lift $ sha3_constants @5 @32 @800)
