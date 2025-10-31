{-# LANGUAGE TemplateHaskell #-}

module KeccakF200
  ( keccakf200
  , topEntity
  ) where

import Clash.Prelude hiding (pi)
import SHA3internal

-- Keccak-f[200]: l=3, w=8, b=200, 18 rounds
-- Small-to-mid-size variant for synthesis testing

type State200 = State 200

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

-- | Keccak-f[200] - full 18 rounds
-- w = 8 bits, l = 3, rounds = 12 + 2*3 = 18
keccakf200 :: State200 -> State200
keccakf200 = keccakp @200 @18 @3 @8 c where
  c = $(lift $ sha3_constants @3 @8 @200)

-- Hardware top entity - one round of Keccak-f[200]
{-# ANN topEntity
  (Synthesize
    { t_name = "Keccakf200_OneRound"
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
topEntity = exposeClockResetEnable $ fmap (iota c 0 . chi c . pi c . rho c . theta c) where
  c = $(lift $ sha3_constants @3 @8 @200)
