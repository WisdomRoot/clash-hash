{-# LANGUAGE TemplateHaskell #-}

module KeccakF400
  ( keccakf400
  , topEntity
  ) where

import Clash.Prelude hiding (pi)
import SHA3internal

-- Keccak-f[400]: l=4, w=16, b=400, 20 rounds
-- Mid-size variant for synthesis testing

type State400 = State 400

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

-- | Keccak-f[400] - full 20 rounds
-- w = 16 bits, l = 4, rounds = 12 + 2*4 = 20
keccakf400 :: State400 -> State400
keccakf400 = keccakp @400 @20 @4 @16 c where
  c = $(lift $ sha3_constants @4 @16 @400)

-- Hardware top entity - one round of Keccak-f[400]
{-# ANN topEntity
  (Synthesize
    { t_name = "Keccakf400_OneRound"
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
          -> Signal System State400
          -> Signal System State400
topEntity = exposeClockResetEnable $ fmap (iota c 0 . chi c . pi c . rho c . theta c) where
  c = $(lift $ sha3_constants @4 @16 @400)
