{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined
  ( HashMode (..),
    sponge,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Sponge.XOR qualified as XOR

type MsgBits = 64

type DigestBits = 64

data SeenTLAST
  = SeenTLASTAndPadded -- final block has been absorbed and padded
  | SeenTLASTNotPadded -- final block has been absorbed but not yet padded
  | NotSeenTLAST -- final block not yet absorbed
  deriving (Show, Eq, Generic, NFDataX)

-- | Phases of the sponge operation
data Phase
  = Absorb (Index 17)
  | Permute
      (Index 24)
      SeenTLAST
  | Squeeze (Index 4)
  deriving
    ( Show,
      Eq,
      Generic,
      NFDataX
    )

-- | Internal state of the sponge
--   Note: separating `Phase` from the BitVector state would significantly reduce the size of the multiplexers
data State
  = State Phase (BitVector 1600)
  deriving
    ( Show,
      Eq,
      Generic,
      NFDataX
    )

complementAt :: Index 1600 -> BitVector 1600 -> BitVector 1600
complementAt i state = replaceBit i (complement (state ! i)) state

-- | Padding function + XOR, flips 3 bits depending on the current beatCounter
padSHA3 :: Index 17 -> BitVector 1600 -> BitVector 1600
padSHA3 0 = complementAt 512 . complementAt 1533 . complementAt 1534
padSHA3 1 = complementAt 512 . complementAt 1469 . complementAt 1470
padSHA3 2 = complementAt 512 . complementAt 1405 . complementAt 1406
padSHA3 3 = complementAt 512 . complementAt 1341 . complementAt 1342
padSHA3 4 = complementAt 512 . complementAt 1277 . complementAt 1278
padSHA3 5 = complementAt 512 . complementAt 1213 . complementAt 1214
padSHA3 6 = complementAt 512 . complementAt 1149 . complementAt 1150
padSHA3 7 = complementAt 512 . complementAt 1085 . complementAt 1086
padSHA3 8 = complementAt 512 . complementAt 1021 . complementAt 1022
padSHA3 9 = complementAt 512 . complementAt 957 . complementAt 958
padSHA3 10 = complementAt 512 . complementAt 893 . complementAt 894
padSHA3 11 = complementAt 512 . complementAt 829 . complementAt 830
padSHA3 12 = complementAt 512 . complementAt 765 . complementAt 766
padSHA3 13 = complementAt 512 . complementAt 701 . complementAt 702
padSHA3 14 = complementAt 512 . complementAt 637 . complementAt 638
padSHA3 15 = complementAt 512 . complementAt 573 . complementAt 574
padSHA3 _ = complementAt 512 . complementAt 1597 . complementAt 1598 -- special case for a whole 1088-bit padding

padSHAKE :: Index 17 -> BitVector 1600 -> BitVector 1600
padSHAKE 0 = complementAt 512 . complementAt 1533 . complementAt 1534
padSHAKE 1 = complementAt 512 . complementAt 1469 . complementAt 1470
padSHAKE 2 = complementAt 512 . complementAt 1405 . complementAt 1406
padSHAKE 3 = complementAt 512 . complementAt 1341 . complementAt 1342
padSHAKE 4 = complementAt 512 . complementAt 1277 . complementAt 1278
padSHAKE 5 = complementAt 512 . complementAt 1213 . complementAt 1214
padSHAKE 6 = complementAt 512 . complementAt 1149 . complementAt 1150
padSHAKE 7 = complementAt 512 . complementAt 1085 . complementAt 1086
padSHAKE 8 = complementAt 512 . complementAt 1021 . complementAt 1022
padSHAKE 9 = complementAt 512 . complementAt 957 . complementAt 958
padSHAKE 10 = complementAt 512 . complementAt 893 . complementAt 894
padSHAKE 11 = complementAt 512 . complementAt 829 . complementAt 830
padSHAKE 12 = complementAt 512 . complementAt 765 . complementAt 766
padSHAKE 13 = complementAt 512 . complementAt 701 . complementAt 702
padSHAKE 14 = complementAt 512 . complementAt 637 . complementAt 638
padSHAKE 15 = complementAt 512 . complementAt 573 . complementAt 574
padSHAKE _ = complementAt 512 . complementAt 1597 . complementAt 1598 -- special case for a whole 1088-bit padding

data HashMode = SHA3 | SHAKE

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
  HashMode -> -- SHAKE256 or SHA3-256 mode
  (Index 24 -> BitVector 1600 -> BitVector 1600) -> -- Permutation function
  Signal dom (AXI4Stream MsgBits, Bool) -> -- Input message, output tready
  Signal dom (AXI4Stream DigestBits, Bool) -- Output digest (AXI4-Stream), input tready
sponge mode permute = mealy step (State (Absorb 0) 0)
  where
    step :: State -> (AXI4Stream MsgBits, Bool) -> (State, (AXI4Stream DigestBits, Bool))
    step (State (Absorb counter) state) (input, _tready)
      | not (tvalid input) = (State (Absorb counter) state, (idleAXI4Stream, True)) -- wait for valid input
      | tlast input && counter < 16 =
          let state' = XOR.staticXOR state (tdata input) counter
              padded = case mode of
                  SHA3 -> padSHA3 counter state'
                  SHAKE -> padSHAKE counter state'
           in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
      | tlast input && counter >= 16 =
          let state' = XOR.staticXOR state (tdata input) counter
           in (State (Permute 0 SeenTLASTNotPadded) state', (idleAXI4Stream, False))
      | counter < 16 =
          let state' = XOR.staticXOR state (tdata input) counter
           in (State (Absorb (counter + 1)) state', (idleAXI4Stream, True))
      | otherwise =
          let state' = XOR.staticXOR state (tdata input) counter
           in (State (Permute 0 NotSeenTLAST) state', (idleAXI4Stream, False))
    step (State (Permute counter seenTLAST) state) (_msg, tready) =
      let state' = permute counter state
       in if counter == 23
            then case seenTLAST of
              SeenTLASTAndPadded ->
                let outStream = AXI4Stream {tdata = slice (SNat @1599) (SNat @1536) state', tvalid = True, tlast = False}
                    nextState = if tready then State (Squeeze 1) state' else State (Squeeze 0) state'
                in (nextState, (outStream, False))
                -- (State (Squeeze 0) state', (idleAXI4Stream, False)) -- go to squeeze phase
              SeenTLASTNotPadded ->
                let padded = case mode of
                      SHA3 -> padSHA3 16 state'
                      SHAKE -> padSHAKE 16 state'
                 in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False)) -- apply 1088-bit padding, and then permute again
              NotSeenTLAST -> (State (Absorb 0) state', (idleAXI4Stream, True)) -- go back to absorb phase
            else (State (Permute (counter + 1) seenTLAST) state', (idleAXI4Stream, False))
    -- Squeeze phase with backpressure: only advance if tready is True
    step (State (Squeeze 0) state) (_msg, tready) =
      let outStream = AXI4Stream {tdata = slice (SNat @1599) (SNat @1536) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 1) state else State (Squeeze 0) state
       in (nextState, (outStream, False))
    step (State (Squeeze 1) state) (_msg, tready) =
      let outStream = AXI4Stream {tdata = slice (SNat @1535) (SNat @1472) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 2) state else State (Squeeze 1) state
       in (nextState, (outStream, False))
    step (State (Squeeze 2) state) (_msg, tready) =
      let outStream = AXI4Stream {tdata = slice (SNat @1471) (SNat @1408) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 3) state else State (Squeeze 2) state
       in (nextState, (outStream, False))
    step (State (Squeeze _) state) (_msg, tready) =
      let outStream = AXI4Stream {tdata = slice (SNat @1407) (SNat @1344) state, tvalid = True, tlast = True}
          nextState = if tready then State (Absorb 0) 0 else State (Squeeze 3) state
       in (nextState, (outStream, False))
