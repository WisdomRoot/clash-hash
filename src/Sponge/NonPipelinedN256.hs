{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | 256-bit streaming variant of the non-pipelined sponge (normal bit order)
-- with 256-bit output beats.
module Sponge.NonPipelinedN256
  ( DigestBits,
    State (..),
    Phase (..),
    SeenTLAST (..),
    complementAt,
    absorb,
    permute,
    squeeze,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)

type DigestBits = 256

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

{-# INLINE absorb #-}
absorb ::
  (KnownNat k) =>
  (Index k -> BitVector 1600 -> BitVector 1600) ->
  (BitVector 1600 -> BitVector input -> Index k -> BitVector 1600) ->
  Index k ->
  BitVector 1600 ->
  AXI4Stream input ->
  Bool ->
  (State k n, (AXI4Stream DigestBits, Bool))
absorb pad xorFn counter state input flush
  | flush && counter == 0 =
      -- Empty input: use wildcard padding (whole 1088-bit padding)
      let padded = pad maxBound state
       in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
  | not (tvalid input) = (State (Absorb counter) state, (idleAXI4Stream, True)) -- wait for valid input
  | tlast input && counter < maxBound =
      let state' = xorFn state (tdata input) counter
          padded = pad counter state'
       in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
  | tlast input && otherwise =
      let state' = xorFn state (tdata input) counter
       in (State (Permute 0 SeenTLASTNotPadded) state', (idleAXI4Stream, False))
  | counter < maxBound =
      let state' = xorFn state (tdata input) counter
       in (State (Absorb (counter + 1)) state', (idleAXI4Stream, True))
  | otherwise =
      let state' = xorFn state (tdata input) counter
       in (State (Permute 0 NotSeenTLAST) state', (idleAXI4Stream, False))

-- | Adding the INLINE pragma would cause the area to swell a bit
permute ::
  (KnownNat k, KnownNat n) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) ->
  (Index k -> BitVector 1600 -> BitVector 1600) ->
  (BitVector 1600 -> BitVector 256) ->
  Index 24 ->
  SeenTLAST ->
  BitVector 1600 ->
  Bool ->
  (State k (Index n), (AXI4Stream DigestBits, Bool))
permute permModule pad cut counter seenTLAST state tready =
  let state' = permModule counter state
   in if counter == maxBound
        then case seenTLAST of
          SeenTLASTAndPadded ->
            let outStream = AXI4Stream {tdata = cut state', tvalid = True, tlast = False}
                nextState = if tready then State (Squeeze 1) state' else State (Squeeze 0) state'
             in (nextState, (outStream, False))
          SeenTLASTNotPadded ->
            let padded = pad maxBound state'
             in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
          NotSeenTLAST -> (State (Absorb 0) state', (idleAXI4Stream, True))
        else (State (Permute (counter + 1) seenTLAST) state', (idleAXI4Stream, False))

{-# INLINE squeeze #-}
squeeze ::
  (KnownNat k, KnownNat n) =>
  (BitVector 1600 -> Index n -> BitVector 256) ->
  Index n ->
  BitVector 1600 ->
  Bool ->
  (State k (Index n), (AXI4Stream 256, Bool))
squeeze squeezeSlice counter state tready =
  let isLast = counter == maxBound
      outStream = AXI4Stream {tdata = squeezeSlice state counter, tvalid = True, tlast = isLast}
      nextState
        | not tready = State (Squeeze counter) state
        | isLast = State (Absorb 0) 0
        | otherwise = State (Squeeze (counter + 1)) state
   in (nextState, (outStream, False))