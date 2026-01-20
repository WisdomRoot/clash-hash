{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Component.SampleNTT
  ( topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation.Perm qualified as Perm
import Sponge.NonPipelined
import Sponge.XOR qualified as XOR


{-# ANN
  topEntity
  ( Synthesize
      { t_name = "Component_SampleNTT",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "MSG_34B",
            PortName "DIGEST_TREADY"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "DIGEST_TDATA",
              PortName "DIGEST_TVALID",
              PortName "DIGEST_TLAST"
            ]
      }
  )
  #-}
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 272) ->
  Signal System Bool ->
  Signal System (AXI4Stream 12, Bool)
topEntity clk rst en msgSig treadySig =
  withClockResetEnable clk rst en
    $ hashFixed34 msgSig treadySig

-- | Fixed-length SHAKE128 for SampleNTT.
--   Consumes a fixed 34-byte message, applies SHAKE padding internally,
--   then streams 64-bit output beats.
hashFixed34 ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Signal dom (BitVector 272) ->
  Signal dom Bool ->
  Signal dom (AXI4Stream 12, Bool)
hashFixed34 msgSig treadySig = mealy step HSInit (bundle (msgSig, treadySig))
  where
    step ::
      HashState ->
      (BitVector 272, Bool) ->
      (HashState, (AXI4Stream 12, Bool))
    step st (inputMsg, tready) =
      case st of
        HSInit ->
          let initState = buildState (unpack inputMsg :: Vec 34 (BitVector 8))
           in (HSPermute 0 initState, (idleAXI4Stream, True))
        HSPermute roundIdx state ->
          let state' = Perm.keccakF1600Round roundIdx state
           in if roundIdx == maxBound
                then (HSSqueeze 0 0 state', (idleAXI4Stream, False))
                else (HSPermute (roundIdx + 1) state', (idleAXI4Stream, False))
        HSSqueeze outIdx ptr state ->
          let word = squeezeSlice outIdx state
              coeff = coeffFromWord word ptr
              outStream =
                AXI4Stream
                  { tdata = pack (coeff :: Unsigned 12),
                    tvalid = True,
                    tlast = False
                  }
              nextState =
                if tready
                  then
                    if ptr == 4
                      then
                        if outIdx == maxBound
                          then HSPermute 0 state
                          else HSSqueeze (outIdx + 1) 0 state
                      else HSSqueeze outIdx (ptr + 1) state
                  else HSSqueeze outIdx ptr state
           in (nextState, (outStream, False))

data HashState
  = HSInit
  | HSPermute (Index 24) (BitVector 1600)
  | HSSqueeze (Index 21) (Index 5) (BitVector 1600)
  deriving (Show, Eq, Generic, NFDataX)

-- | Build initial Keccak state from the 34-byte message with SHAKE padding.
buildState :: Vec 34 (BitVector 8) -> BitVector 1600
buildState msg =
  let zeroPad :: Vec 132 (BitVector 8)
      zeroPad = repeat 0
      padded :: Vec 168 (BitVector 8)
      padded = msg ++ (0x1F :> zeroPad) ++ (0x80 :> Nil)
      rateWords :: Vec 21 (BitVector 64)
      rateWords = map wordFromBytes (unconcat d8 padded)
   in foldl (\st (i, w) -> XOR.staticXOR128 st w i) 0 (imap (,) rateWords)

-- | Convert 8 bytes into a 64-bit word using the same packing as the harness.
wordFromBytes :: Vec 8 (BitVector 8) -> BitVector 64
wordFromBytes bytes =
  let bits :: Vec 64 Bit
      bits = concatMap (reverse . unpack) bytes
   in pack bits

toU12 :: BitVector 8 -> Unsigned 12
toU12 b = resize (unpack b :: Unsigned 8)

bytesFromWord :: BitVector 64 -> Vec 8 (BitVector 8)
bytesFromWord w = map (byteFromWord w) indicesI

coeffFromWord :: BitVector 64 -> Index 5 -> Unsigned 12
coeffFromWord w ptr =
  let rawBytes = bytesFromWord w
      bytes = map bitReverse8 rawBytes
      ptr16 :: Index 16
      ptr16 = resize ptr
   in coeffFromBytes bytes ptr16

bitReverse8 :: BitVector 8 -> BitVector 8
bitReverse8 b = pack (reverse (unpack b :: Vec 8 Bit))

byteFromWord :: BitVector 64 -> Index 8 -> BitVector 8
byteFromWord w k =
  let base = fromIntegral k * 8
      bits :: Vec 8 Bit
      bits = map (\i -> boolToBit (testBit w (63 - (base + fromIntegral i)))) indicesI
   in pack bits

coeffFromBytes :: Vec 8 (BitVector 8) -> Index 16 -> Unsigned 12
coeffFromBytes bytes pointer =
  let i0 = 0 :: Index 8
      i1 = 1 :: Index 8
      i2 = 2 :: Index 8
      i3 = 3 :: Index 8
      i4 = 4 :: Index 8
      i5 = 5 :: Index 8
      i6 = 6 :: Index 8
      i7 = 7 :: Index 8
      b0 = bytes !! i0
      b1 = bytes !! i1
      b2 = bytes !! i2
      b3 = bytes !! i3
      b4 = bytes !! i4
      b5 = bytes !! i5
      b6 = bytes !! i6
      b7 = bytes !! i7
      d1' x y = toU12 x + shiftL (resize (unpack (y .&. 0x0F) :: Unsigned 8)) 8
      d2' x y = resize (unpack (x `shiftR` 4) :: Unsigned 8) + shiftL (toU12 y) 4
   in case pointer of
        0 -> d1' b0 b1
        1 -> d2' b1 b2
        2 -> d1' b3 b4
        3 -> d2' b4 b5
        _ -> d1' b6 b7

type RateBeats = 21

type PadBeats = 21

-- | Padding function + XOR, flips 5 domain bits plus pad bit at bit 256.
pad :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad 0 =
  complementAt 256
    . complementAt 1531
    . complementAt 1532
    . complementAt 1533
    . complementAt 1534
    . complementAt 1535
pad 1 =
  complementAt 256
    . complementAt 1467
    . complementAt 1468
    . complementAt 1469
    . complementAt 1470
    . complementAt 1471
pad 2 =
  complementAt 256
    . complementAt 1403
    . complementAt 1404
    . complementAt 1405
    . complementAt 1406
    . complementAt 1407
pad 3 =
  complementAt 256
    . complementAt 1339
    . complementAt 1340
    . complementAt 1341
    . complementAt 1342
    . complementAt 1343
pad 4 =
  complementAt 256
    . complementAt 1275
    . complementAt 1276
    . complementAt 1277
    . complementAt 1278
    . complementAt 1279
pad 5 =
  complementAt 256
    . complementAt 1211
    . complementAt 1212
    . complementAt 1213
    . complementAt 1214
    . complementAt 1215
pad 6 =
  complementAt 256
    . complementAt 1147
    . complementAt 1148
    . complementAt 1149
    . complementAt 1150
    . complementAt 1151
pad 7 =
  complementAt 256
    . complementAt 1083
    . complementAt 1084
    . complementAt 1085
    . complementAt 1086
    . complementAt 1087
pad 8 =
  complementAt 256
    . complementAt 1019
    . complementAt 1020
    . complementAt 1021
    . complementAt 1022
    . complementAt 1023
pad 9 =
  complementAt 256
    . complementAt 955
    . complementAt 956
    . complementAt 957
    . complementAt 958
    . complementAt 959
pad 10 =
  complementAt 256
    . complementAt 891
    . complementAt 892
    . complementAt 893
    . complementAt 894
    . complementAt 895
pad 11 =
  complementAt 256
    . complementAt 827
    . complementAt 828
    . complementAt 829
    . complementAt 830
    . complementAt 831
pad 12 =
  complementAt 256
    . complementAt 763
    . complementAt 764
    . complementAt 765
    . complementAt 766
    . complementAt 767
pad 13 =
  complementAt 256
    . complementAt 699
    . complementAt 700
    . complementAt 701
    . complementAt 702
    . complementAt 703
pad 14 =
  complementAt 256
    . complementAt 635
    . complementAt 636
    . complementAt 637
    . complementAt 638
    . complementAt 639
pad 15 =
  complementAt 256
    . complementAt 571
    . complementAt 572
    . complementAt 573
    . complementAt 574
    . complementAt 575
pad 16 =
  complementAt 256
    . complementAt 507
    . complementAt 508
    . complementAt 509
    . complementAt 510
    . complementAt 511
pad 17 =
  complementAt 256
    . complementAt 443
    . complementAt 444
    . complementAt 445
    . complementAt 446
    . complementAt 447
pad 18 =
  complementAt 256
    . complementAt 379
    . complementAt 380
    . complementAt 381
    . complementAt 382
    . complementAt 383
pad 19 =
  complementAt 256
    . complementAt 315
    . complementAt 316
    . complementAt 317
    . complementAt 318
    . complementAt 319
pad _ =
  complementAt 256
    . complementAt 1595
    . complementAt 1596
    . complementAt 1597
    . complementAt 1598
    . complementAt 1599

-- | Extract 64-bit chunks from the Keccak state during squeeze.
squeezeSlice :: Index RateBeats -> BitVector 1600 -> BitVector 64
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
squeezeSlice 16 state = slice (SNat @575) (SNat @512) state
squeezeSlice 17 state = slice (SNat @511) (SNat @448) state
squeezeSlice 18 state = slice (SNat @447) (SNat @384) state
squeezeSlice 19 state = slice (SNat @383) (SNat @320) state
squeezeSlice _ state = slice (SNat @319) (SNat @256) state

-- | Stateful sponge with AXI4-Stream backpressure support.
{-# OPAQUE sponge #-}
sponge ::
  forall dom n.
  ( HiddenClockResetEnable dom,
    KnownNat n,
    n ~ DivRU (MsgBits + 2) 1344,
    MsgBits + 2 <= n * 1344,
    MsgBits + 4 <= n * 1344
  ) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) ->
  Signal dom (AXI4Stream MsgBits, Bool, Bool) ->
  Signal dom (AXI4Stream DigestBits, Bool)
sponge permModule = mealy step (State (Absorb 0) 0)
  where
    step ::
      State PadBeats (Index RateBeats) ->
      (AXI4Stream MsgBits, Bool, Bool) ->
      (State PadBeats (Index RateBeats), (AXI4Stream DigestBits, Bool))
    step (State (Absorb counter) state) (input, _tready, flush) = absorb pad XOR.staticXOR128 counter state input flush
    step (State (Permute counter seenTLAST) state) (_msg, tready, _flush) = permute permModule pad counter seenTLAST state tready
    step (State (Squeeze counter) state) (_msg, tready, _flush)
      | counter == maxBound =
          let outStream = AXI4Stream {tdata = squeezeSlice counter state, tvalid = True, tlast = False}
              nextState =
                if tready
                  then State (Permute 0 SeenTLASTAndPadded) state
                  else State (Squeeze maxBound) state
           in (nextState, (outStream, False))
      | otherwise =
          let outStream = AXI4Stream {tdata = squeezeSlice counter state, tvalid = True, tlast = False}
              nextState =
                if tready
                  then State (Squeeze (counter + 1)) state
                  else State (Squeeze counter) state
           in (nextState, (outStream, False))
