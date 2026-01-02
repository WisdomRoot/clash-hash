{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | KNOWN LIMITATION: This implementation always squeezes the full rate (1088 bits)
-- per squeeze cycle, regardless of requested output length. The test harness
-- compensates by taking only the required number of output beats.
--
-- For production use, add output length tracking to enable early termination:
--   1. Add outputLength input signal to topEntity
--   2. Add output beat counter to FSM state
--   3. Modify squeeze phase to check counter and set tlast appropriately
--   4. Add early termination when counter reaches outputLength
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
padSHAKE 0 =
  complementAt 512
    . complementAt 1531
    . complementAt 1532
    . complementAt 1533
    . complementAt 1534
    . complementAt 1535
padSHAKE 1 =
  complementAt 512
    . complementAt 1467
    . complementAt 1468
    . complementAt 1469
    . complementAt 1470
    . complementAt 1471
padSHAKE 2 =
  complementAt 512
    . complementAt 1403
    . complementAt 1404
    . complementAt 1405
    . complementAt 1406
    . complementAt 1407
padSHAKE 3 =
  complementAt 512
    . complementAt 1339
    . complementAt 1340
    . complementAt 1341
    . complementAt 1342
    . complementAt 1343
padSHAKE 4 =
  complementAt 512
    . complementAt 1275
    . complementAt 1276
    . complementAt 1277
    . complementAt 1278
    . complementAt 1279
padSHAKE 5 =
  complementAt 512
    . complementAt 1211
    . complementAt 1212
    . complementAt 1213
    . complementAt 1214
    . complementAt 1215
padSHAKE 6 =
  complementAt 512
    . complementAt 1147
    . complementAt 1148
    . complementAt 1149
    . complementAt 1150
    . complementAt 1151
padSHAKE 7 =
  complementAt 512
    . complementAt 1083
    . complementAt 1084
    . complementAt 1085
    . complementAt 1086
    . complementAt 1087
padSHAKE 8 =
  complementAt 512
    . complementAt 1019
    . complementAt 1020
    . complementAt 1021
    . complementAt 1022
    . complementAt 1023
padSHAKE 9 =
  complementAt 512
    . complementAt 955
    . complementAt 956
    . complementAt 957
    . complementAt 958
    . complementAt 959
padSHAKE 10 =
  complementAt 512
    . complementAt 891
    . complementAt 892
    . complementAt 893
    . complementAt 894
    . complementAt 895
padSHAKE 11 =
  complementAt 512
    . complementAt 827
    . complementAt 828
    . complementAt 829
    . complementAt 830
    . complementAt 831
padSHAKE 12 =
  complementAt 512
    . complementAt 763
    . complementAt 764
    . complementAt 765
    . complementAt 766
    . complementAt 767
padSHAKE 13 =
  complementAt 512
    . complementAt 699
    . complementAt 700
    . complementAt 701
    . complementAt 702
    . complementAt 703
padSHAKE 14 =
  complementAt 512
    . complementAt 635
    . complementAt 636
    . complementAt 637
    . complementAt 638
    . complementAt 639
padSHAKE 15 =
  complementAt 512
    . complementAt 571
    . complementAt 572
    . complementAt 573
    . complementAt 574
    . complementAt 575
padSHAKE _ =
  complementAt 512
    . complementAt 1595
    . complementAt 1596
    . complementAt 1597
    . complementAt 1598
    . complementAt 1599 -- special case for a whole 1088-bit padding

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
  Signal dom (AXI4Stream MsgBits, Bool, Bool) -> -- Input message, output tready, flush signal
  Signal dom (AXI4Stream DigestBits, Bool) -- Output digest (AXI4-Stream), input tready
sponge mode permute = mealy step (State (Absorb 0) 0)
  where
    step :: State -> (AXI4Stream MsgBits, Bool, Bool) -> (State, (AXI4Stream DigestBits, Bool))
    step (State (Absorb counter) state) (input, _tready, flush)
      | flush && counter == 0 =
          -- Empty input: use wildcard padding (whole 1088-bit padding)
          let padded = case mode of
                SHA3 -> padSHA3 16 state  -- wildcard case for SHA3
                SHAKE -> padSHAKE 16 state  -- wildcard case for SHAKE
           in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
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
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) =
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
    step (State (Squeeze 0) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @1599) (SNat @1536) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 1) state else State (Squeeze 0) state
       in (nextState, (outStream, False))
    step (State (Squeeze 1) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @1535) (SNat @1472) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 2) state else State (Squeeze 1) state
       in (nextState, (outStream, False))
    step (State (Squeeze 2) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @1471) (SNat @1408) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 3) state else State (Squeeze 2) state
       in (nextState, (outStream, False))
    step (State (Squeeze 3) state) (_msg, tready, _flush) = case mode of
      SHA3 ->
        let outStream = AXI4Stream {tdata = slice (SNat @1407) (SNat @1344) state, tvalid = True, tlast = True}
            nextState = if tready then State (Absorb 0) 0 else State (Squeeze 3) state
        in (nextState, (outStream, False))
      SHAKE ->
        let outStream = AXI4Stream {tdata = slice (SNat @1407) (SNat @1344) state, tvalid = True, tlast = False}
            nextState = if tready then State (Squeeze 4) state else State (Squeeze 3) state
        in (nextState, (outStream, False))
    step (State (Squeeze 4) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @1343) (SNat @1280) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 5) state else State (Squeeze 4) state
       in (nextState, (outStream, False))
    step (State (Squeeze 5) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @1279) (SNat @1216) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 6) state else State (Squeeze 5) state
       in (nextState, (outStream, False))
    step (State (Squeeze 6) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @1215) (SNat @1152) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 7) state else State (Squeeze 6) state
       in (nextState, (outStream, False))
    step (State (Squeeze 7) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @1151) (SNat @1088) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 8) state else State (Squeeze 7) state
       in (nextState, (outStream, False))
    step (State (Squeeze 8) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @1087) (SNat @1024) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 9) state else State (Squeeze 8) state
       in (nextState, (outStream, False))
    step (State (Squeeze 9) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @1023) (SNat @960) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 10) state else State (Squeeze 9) state
       in (nextState, (outStream, False))
    step (State (Squeeze 10) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @959) (SNat @896) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 11) state else State (Squeeze 10) state
       in (nextState, (outStream, False))
    step (State (Squeeze 11) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @895) (SNat @832) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 12) state else State (Squeeze 11) state
       in (nextState, (outStream, False))
    step (State (Squeeze 12) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @831) (SNat @768) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 13) state else State (Squeeze 12) state
       in (nextState, (outStream, False))
    step (State (Squeeze 13) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @767) (SNat @704) state, tvalid = True, tlast = False }
          nextState = if tready then State (Squeeze 14) state else State (Squeeze 13) state
       in (nextState, (outStream, False))
    step (State (Squeeze 14) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @703) (SNat @640) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 15) state else State (Squeeze 14) state
       in (nextState, (outStream, False))
    step (State (Squeeze 15) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @639) (SNat @576) state, tvalid = True, tlast = False}
          nextState = if tready then State (Squeeze 16) state else State (Squeeze 15) state
       in (nextState, (outStream, False))
    step (State (Squeeze _) state) (_msg, tready, _flush) =
      let outStream = AXI4Stream {tdata = slice (SNat @575) (SNat @512) state, tvalid = True, tlast = False}
          nextState = if tready then State (Permute 0 SeenTLASTAndPadded) state else State (Squeeze 16) state
       in (nextState, (outStream, False))
    
