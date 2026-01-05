{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SHAKE256
  ( sponge,
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
  | Squeeze (Index 17)
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

-- | Padding function + XOR, flips 5 bits depending on the current beatCounter
pad :: Index 17 -> BitVector 1600 -> BitVector 1600
pad 0 =
  complementAt 512
    . complementAt 1531
    . complementAt 1532
    . complementAt 1533
    . complementAt 1534
    . complementAt 1535
pad 1 =
  complementAt 512
    . complementAt 1467
    . complementAt 1468
    . complementAt 1469
    . complementAt 1470
    . complementAt 1471
pad 2 =
  complementAt 512
    . complementAt 1403
    . complementAt 1404
    . complementAt 1405
    . complementAt 1406
    . complementAt 1407
pad 3 =
  complementAt 512
    . complementAt 1339
    . complementAt 1340
    . complementAt 1341
    . complementAt 1342
    . complementAt 1343
pad 4 =
  complementAt 512
    . complementAt 1275
    . complementAt 1276
    . complementAt 1277
    . complementAt 1278
    . complementAt 1279
pad 5 =
  complementAt 512
    . complementAt 1211
    . complementAt 1212
    . complementAt 1213
    . complementAt 1214
    . complementAt 1215
pad 6 =
  complementAt 512
    . complementAt 1147
    . complementAt 1148
    . complementAt 1149
    . complementAt 1150
    . complementAt 1151
pad 7 =
  complementAt 512
    . complementAt 1083
    . complementAt 1084
    . complementAt 1085
    . complementAt 1086
    . complementAt 1087
pad 8 =
  complementAt 512
    . complementAt 1019
    . complementAt 1020
    . complementAt 1021
    . complementAt 1022
    . complementAt 1023
pad 9 =
  complementAt 512
    . complementAt 955
    . complementAt 956
    . complementAt 957
    . complementAt 958
    . complementAt 959
pad 10 =
  complementAt 512
    . complementAt 891
    . complementAt 892
    . complementAt 893
    . complementAt 894
    . complementAt 895
pad 11 =
  complementAt 512
    . complementAt 827
    . complementAt 828
    . complementAt 829
    . complementAt 830
    . complementAt 831
pad 12 =
  complementAt 512
    . complementAt 763
    . complementAt 764
    . complementAt 765
    . complementAt 766
    . complementAt 767
pad 13 =
  complementAt 512
    . complementAt 699
    . complementAt 700
    . complementAt 701
    . complementAt 702
    . complementAt 703
pad 14 =
  complementAt 512
    . complementAt 635
    . complementAt 636
    . complementAt 637
    . complementAt 638
    . complementAt 639
pad 15 =
  complementAt 512
    . complementAt 571
    . complementAt 572
    . complementAt 573
    . complementAt 574
    . complementAt 575
pad _ =
  complementAt 512
    . complementAt 1595
    . complementAt 1596
    . complementAt 1597
    . complementAt 1598
    . complementAt 1599 -- special case for a whole 1088-bit padding

-- | Squeeze phase bit slicing helper: extracts 64-bit chunks from the Keccak state
squeezeSlice :: Index 17 -> BitVector 1600 -> BitVector 64
squeezeSlice 0 state = slice (SNat @1599) (SNat @1536) state
squeezeSlice 1 state = slice (SNat @1535) (SNat @1472) state
squeezeSlice 2 state = slice (SNat @1471) (SNat @1408) state
squeezeSlice 3 state = slice (SNat @1407) (SNat @1344) state
squeezeSlice 4 state = slice (SNat @1343) (SNat @1280) state
squeezeSlice 5 state = slice (SNat @1279) (SNat @1216) state
squeezeSlice 6 state = slice (SNat @1215) (SNat @1152) state
squeezeSlice 7 state = slice (SNat @1151) (SNat @1088) state
squeezeSlice 8 state = slice (SNat @1087) (SNat @1024) state
squeezeSlice 9 state = slice (SNat @1023) (SNat @960) state
squeezeSlice 10 state = slice (SNat @959) (SNat @896) state
squeezeSlice 11 state = slice (SNat @895) (SNat @832) state
squeezeSlice 12 state = slice (SNat @831) (SNat @768) state
squeezeSlice 13 state = slice (SNat @767) (SNat @704) state
squeezeSlice 14 state = slice (SNat @703) (SNat @640) state
squeezeSlice 15 state = slice (SNat @639) (SNat @576) state
squeezeSlice _ state = slice (SNat @575) (SNat @512) state

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
sponge permute = mealy step (State (Absorb 0) 0)
  where
    step :: State -> (AXI4Stream MsgBits, Bool, Bool) -> (State, (AXI4Stream DigestBits, Bool))
    step (State (Absorb counter) state) (input, _tready, flush)
      | flush && counter == 0 =
          -- Empty input: use wildcard padding (whole 1088-bit padding)
          let padded = pad 16 state -- wildcard case for SHAKE
           in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
      | not (tvalid input) = (State (Absorb counter) state, (idleAXI4Stream, True)) -- wait for valid input
      | tlast input && counter < 16 =
          let state' = XOR.staticXOR state (tdata input) counter
              padded = pad counter state'
           in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
      | tlast input && otherwise =
          let state' = XOR.staticXOR state (tdata input) counter
           in (State (Permute 0 SeenTLASTNotPadded) state', (idleAXI4Stream, False))
      | counter < 16 =
          let state' = XOR.staticXOR state (tdata input) counter
           in (State (Absorb (counter + 1)) state', (idleAXI4Stream, True))
      | otherwise =
          let state' = XOR.staticXOR state (tdata input) counter
           in (State (Permute 0 NotSeenTLAST) state', (idleAXI4Stream, False))
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) =
      let state' = permute counter state
       in if counter == 23
            then case seenTLAST of
              SeenTLASTAndPadded ->
                let outStream = AXI4Stream {tdata = slice (SNat @1599) (SNat @1536) state', tvalid = True, tlast = False}
                    nextState = if tready then State (Squeeze 1) state' else State (Squeeze 0) state'
                 in (nextState, (outStream, False))
              SeenTLASTNotPadded ->
                let padded = pad 16 state'
                 in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False)) -- apply 1088-bit padding, and then permute again
              NotSeenTLAST -> (State (Absorb 0) state', (idleAXI4Stream, True)) -- go back to absorb phase
            else (State (Permute (counter + 1) seenTLAST) state', (idleAXI4Stream, False))
    step (State (Squeeze counter) state) (_msg, tready, _flush)
      | counter == 16 =
          let outStream = AXI4Stream {tdata = squeezeSlice 16 state, tvalid = True, tlast = False}
              nextState = if tready then State (Permute 0 SeenTLASTAndPadded) state else State (Squeeze 16) state
           in (nextState, (outStream, False))
      | otherwise =
          let outStream = AXI4Stream {tdata = squeezeSlice counter state, tvalid = True, tlast = False}
              nextState = if tready then State (Squeeze (counter + 1)) state else State (Squeeze counter) state
           in (nextState, (outStream, False))