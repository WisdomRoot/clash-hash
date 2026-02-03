{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SHA3256N
  ( sponge,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import TH (mkRead)
import Sponge.NonPipelinedN
import Sponge.XOR qualified as XOR

-- | Padding function + XOR, flips 3 bits depending on the current beatCounter
pad :: Index 17 -> BitVector 1600 -> BitVector 1600
pad 0 = complementAt 1087 . complementAt 66 . complementAt 65
pad 1 = complementAt 1087 . complementAt 130 . complementAt 129
pad 2 = complementAt 1087 . complementAt 194 . complementAt 193
pad 3 = complementAt 1087 . complementAt 258 . complementAt 257
pad 4 = complementAt 1087 . complementAt 322 . complementAt 321
pad 5 = complementAt 1087 . complementAt 386 . complementAt 385
pad 6 = complementAt 1087 . complementAt 450 . complementAt 449
pad 7 = complementAt 1087 . complementAt 514 . complementAt 513
pad 8 = complementAt 1087 . complementAt 578 . complementAt 577
pad 9 = complementAt 1087 . complementAt 642 . complementAt 641
pad 10 = complementAt 1087 . complementAt 706 . complementAt 705
pad 11 = complementAt 1087 . complementAt 770 . complementAt 769
pad 12 = complementAt 1087 . complementAt 834 . complementAt 833
pad 13 = complementAt 1087 . complementAt 898 . complementAt 897
pad 14 = complementAt 1087 . complementAt 962 . complementAt 961
pad 15 = complementAt 1087 . complementAt 1026 . complementAt 1025
pad _ = complementAt 1087 . complementAt 2 . complementAt 1 -- special case for a whole 1088-bit padding

-- | Squeeze phase bit slicing helper: extracts 64-bit chunks from the Keccak state
$(mkRead "squeezeSlice" 1600 [(0, 0, 64), (1, 64, 64), (2, 128, 64), (3, 192, 64)])

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
    step (State (Absorb counter) state) (input, _tready, flush) = absorb pad XOR.staticXOR256' counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule pad counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush)
      | counter == maxBound =
          let outStream = AXI4Stream {tdata = squeezeSlice state counter, tvalid = True, tlast = True}
              nextState = if tready then State (Absorb 0) 0 else State (Squeeze counter) state
           in (nextState, (outStream, False))
      | otherwise =
          let outStream = AXI4Stream {tdata = squeezeSlice state counter, tvalid = True, tlast = False}
              nextState = if tready then State (Squeeze (counter + 1)) state else State (Squeeze counter) state
           in (nextState, (outStream, False))
