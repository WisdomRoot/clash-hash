{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SHA3256
  ( sponge,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Sponge.NonPipelined
import Sponge.XOR qualified as XOR

-- | Padding function + XOR, flips 3 bits depending on the current beatCounter
pad :: Index 17 -> BitVector 1600 -> BitVector 1600
pad 0 = complementAt 512 . complementAt 1533 . complementAt 1534
pad 1 = complementAt 512 . complementAt 1469 . complementAt 1470
pad 2 = complementAt 512 . complementAt 1405 . complementAt 1406
pad 3 = complementAt 512 . complementAt 1341 . complementAt 1342
pad 4 = complementAt 512 . complementAt 1277 . complementAt 1278
pad 5 = complementAt 512 . complementAt 1213 . complementAt 1214
pad 6 = complementAt 512 . complementAt 1149 . complementAt 1150
pad 7 = complementAt 512 . complementAt 1085 . complementAt 1086
pad 8 = complementAt 512 . complementAt 1021 . complementAt 1022
pad 9 = complementAt 512 . complementAt 957 . complementAt 958
pad 10 = complementAt 512 . complementAt 893 . complementAt 894
pad 11 = complementAt 512 . complementAt 829 . complementAt 830
pad 12 = complementAt 512 . complementAt 765 . complementAt 766
pad 13 = complementAt 512 . complementAt 701 . complementAt 702
pad 14 = complementAt 512 . complementAt 637 . complementAt 638
pad 15 = complementAt 512 . complementAt 573 . complementAt 574
pad _ = complementAt 512 . complementAt 1597 . complementAt 1598 -- special case for a whole 1088-bit padding

-- | Squeeze phase bit slicing helper: extracts 64-bit chunks from the Keccak state
squeezeSlice :: Index 4 -> BitVector 1600 -> BitVector 64
squeezeSlice 0 state = slice (SNat @1599) (SNat @1536) state
squeezeSlice 1 state = slice (SNat @1535) (SNat @1472) state
squeezeSlice 2 state = slice (SNat @1471) (SNat @1408) state
squeezeSlice _ state = slice (SNat @1407) (SNat @1344) state

-- | Stateful sponge with AXI4-Stream backpressure support
{-# OPAQUE sponge #-}
sponge ::
  forall dom n.
  ( HiddenClockResetEnable dom,
    KnownNat n,
    n ~ DivRU (MsgBits + 2) 1088,
    MsgBits + 2 <= n * 1088,
    MsgBits + 4 <= n * 1088
  ) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (AXI4Stream MsgBits, Bool, Bool) -> -- Input message, output tready, flush signal
  Signal dom (AXI4Stream DigestBits, Bool) -- Output digest (AXI4-Stream), input tready
sponge permModule = mealy step (State (Absorb 0) 0)
  where
    step :: State 17 (Index 4) -> (AXI4Stream MsgBits, Bool, Bool) -> (State 17 (Index 4), (AXI4Stream DigestBits, Bool))
    step (State (Absorb counter) state) (input, _tready, flush) = absorb pad XOR.staticXOR256 counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule pad counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush)
      | counter == 3 =
          let outStream = AXI4Stream {tdata = squeezeSlice 3 state, tvalid = True, tlast = True}
              nextState = if tready then State (Absorb 0) 0 else State (Squeeze 3) state
           in (nextState, (outStream, False))
      | otherwise =
          let outStream = AXI4Stream {tdata = squeezeSlice counter state, tvalid = True, tlast = False}
              nextState = if tready then State (Squeeze (counter + 1)) state else State (Squeeze counter) state
           in (nextState, (outStream, False))
