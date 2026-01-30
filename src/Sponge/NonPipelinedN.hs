{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | 64-bit streaming variant of the non-pipelined sponge (normal bit order).
module Sponge.NonPipelinedN
  ( MsgBits,
    DigestBits,
    State (..),
    Phase (..),
    SeenTLAST (..),
    complementAt,
    absorb,
    permute
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)

type MsgBits = 64

type DigestBits = 64

data SeenTLAST
  = SeenTLASTAndPadded -- final block has been absorbed and padded
  | SeenTLASTNotPadded -- final block has been absorbed but not yet padded
  | NotSeenTLAST -- final block not yet absorbed
  deriving (Show, Eq, Generic, NFDataX)

-- | Phases of the sponge operation
data Phase k q
  = Absorb (Index k)
  | Permute
      (Index 24)
      SeenTLAST
  | Squeeze q
  deriving
    ( Show,
      Eq,
      Generic,
      NFDataX
    )

-- | Internal state of the sponge
--   Note: separating `Phase` from the BitVector state would significantly reduce the size of the multiplexers
data State k n
  = State (Phase k n) (BitVector 1600)
  deriving
    ( Show,
      Eq,
      Generic,
      NFDataX
    )

complementAt :: Index 1600 -> BitVector 1600 -> BitVector 1600
complementAt i state = replaceBit i (complement (state ! i)) state

absorb ::
  (KnownNat k) =>
  (Index k -> BitVector 1600 -> BitVector 1600) ->
  (BitVector 1600 -> BitVector MsgBits -> Index k -> BitVector 1600) ->
  Index k ->
  BitVector 1600 ->
  AXI4Stream MsgBits ->
  Bool ->
  (State k n, (AXI4Stream DigestBits, Bool))
absorb pad xorFn counter state input flush
  | flush && counter == 0 =
      -- Empty input: use wildcard padding (whole 1088-bit padding)
      let padded = pad maxAbsorbBeat state
       in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
  | not (tvalid input) = (State (Absorb counter) state, (idleAXI4Stream, True)) -- wait for valid input
  | tlast input && counter < maxAbsorbBeat =
      let state' = xorFn state (tdata input) counter
          padded = pad counter state'
       in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
  | tlast input && otherwise =
      let state' = xorFn state (tdata input) counter
       in (State (Permute 0 SeenTLASTNotPadded) state', (idleAXI4Stream, False))
  | counter < maxAbsorbBeat =
      let state' = xorFn state (tdata input) counter
       in (State (Absorb (counter + 1)) state', (idleAXI4Stream, True))
  | otherwise =
      let state' = xorFn state (tdata input) counter
       in (State (Permute 0 NotSeenTLAST) state', (idleAXI4Stream, False))
  where
    maxAbsorbBeat = maxBound

permute ::
  (KnownNat k, KnownNat n) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) ->
  (Index k -> BitVector 1600 -> BitVector 1600) ->
  Index 24 ->
  SeenTLAST ->
  BitVector 1600 ->
  Bool ->
  (State k (Index n), (AXI4Stream DigestBits, Bool))
permute permModule pad counter seenTLAST state tready =
  let state' = permModule counter state
      outData = slice (SNat @63) (SNat @0) state'
   in if counter == 23
        then case seenTLAST of
          SeenTLASTAndPadded ->
            let outStream = AXI4Stream {tdata = outData, tvalid = True, tlast = False}
                nextState = if tready then State (Squeeze 1) state' else State (Squeeze 0) state'
             in (nextState, (outStream, False))
          SeenTLASTNotPadded ->
            let padded = pad maxBound state'
             in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
          NotSeenTLAST -> (State (Absorb 0) state', (idleAXI4Stream, True))
        else (State (Permute (counter + 1) seenTLAST) state', (idleAXI4Stream, False))
