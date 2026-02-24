{-# LANGUAGE TemplateHaskell #-}

module Component.SampleNTT512
  ( i272o24l0,
    i272o24l1,
    Lookahead (..),
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)
import TH (mkRead)
import Prelude qualified as P

data Lookahead = Lookahead0 | Lookahead1
  deriving (Show, Eq, Generic, NFDataX)

data State
  = Idle
  | Permute (Index 24) (BitVector 1600) (Maybe (BitVector 12)) -- pending single valid coeff carried across permute
  | Squeeze (Index 112) (BitVector 1600) (Maybe (BitVector 12)) -- pending coeff carried across stalls/block boundary
  deriving (Show, Eq, Generic, NFDataX)

-- | Extract 24-bit coefficient pair from state (pattern matched on all 56 indices)
$( mkRead
     "squeezeCoeff24"
     1600
     [ (i, i * 24, 24) | i <- [0 .. 55] ]
 )

{-# INLINE squeezeCoeff24 #-}

$( mkRead
     "squeezeCoeff12"
     1600
     [ (i, i * 12, 12) | i <- [0 .. 111] ]
 )

{-# INLINE squeezeCoeff12 #-}

-- | Clean hashWith function for fixed 34-byte input with AXI4-Stream input handshaking
hashWith ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Lookahead ->
  Signal dom (AXI4Stream 272, Bool) ->
  Signal dom (AXI4Stream 24, Bool)
hashWith lookahead = mealy step (Idle :: State)
  where
    step ::
      State ->
      (AXI4Stream 272, Bool) ->
      (State, (AXI4Stream 24, Bool))
    step st (AXI4Stream inputMsg msgValid _, tready) =
      case st of
        Idle ->
          -- SEED_TREADY is True, waiting for SEED_TVALID
          if msgValid
            then (Permute 0 (absorb34 inputMsg) Nothing, (idleAXI4Stream, False))
            else (Idle, (idleAXI4Stream, True))
        Permute roundIdx state buffer ->
          let state' = Permutation.keccakF1600 roundIdx state
           in if roundIdx == maxBound
                then (Squeeze 0 state' buffer, (idleAXI4Stream, False))
                else (Permute (roundIdx + 1) state' buffer, (idleAXI4Stream, False))
        Squeeze index state buffer ->
          case lookahead of
            Lookahead0 ->
              let idxInt = fromEnum index
                  pairIdx = fromIntegral (idxInt `div` 2) :: Index 56
                  pair = squeezeCoeff24 state pairIdx
                  c0 = slice (SNat @11) (SNat @0) pair
                  c1 = slice (SNat @23) (SNat @12) pair
                  v0 = (unpack c0 :: Unsigned 12) < (3329 :: Unsigned 12)
                  v1 = (unpack c1 :: Unsigned 12) < (3329 :: Unsigned 12)
                  wrap = idxInt >= 110
                  nextIdx = if wrap then 0 else fromIntegral (idxInt + 2)
                  (pairReady, tdataOut, nextBuffer) =
                    case (buffer, v0, v1) of
                      (Just b, True, True) -> (True, c0 ++# b, Just c1)
                      (Just b, True, False) -> (True, c0 ++# b, Nothing)
                      (Just b, False, True) -> (True, c1 ++# b, Nothing)
                      (Just b, False, False) -> (False, 0, Just b)
                      (Nothing, True, True) -> (True, c1 ++# c0, Nothing)
                      (Nothing, True, False) -> (False, 0, Just c0)
                      (Nothing, False, True) -> (False, 0, Just c1)
                      (Nothing, False, False) -> (False, 0, Nothing)
                  outStream =
                    AXI4Stream
                      { tdata = tdataOut,
                        tvalid = pairReady,
                        tlast = False
                      }
                  nextState =
                    if tready
                      then
                        if wrap
                          then Permute 0 state nextBuffer
                          else Squeeze nextIdx state nextBuffer
                      else Squeeze index state buffer
               in (nextState, (outStream, False))
            Lookahead1 ->
              let idxInt = fromEnum index
                  remaining = 112 - idxInt
                  c0 = squeezeCoeff12 state index
                  c1 = if remaining P.> 1
                    then Just (squeezeCoeff12 state (fromIntegral (idxInt + 1)))
                    else Nothing
                  c2 = if remaining P.> 2
                    then Just (squeezeCoeff12 state (fromIntegral (idxInt + 2)))
                    else Nothing
                  v0 = (unpack c0 :: Unsigned 12) < (3329 :: Unsigned 12)
                  v1 = case c1 of
                    Just v -> (unpack v :: Unsigned 12) < (3329 :: Unsigned 12)
                    Nothing -> False
                  v2 = case c2 of
                    Just v -> (unpack v :: Unsigned 12) < (3329 :: Unsigned 12)
                    Nothing -> False
                  (consumeN, pairReady, tdataOut, nextBuffer) =
                    case buffer of
                      Nothing ->
                        case (v0, v1, v2) of
                          (True, True, _) -> (2, True, (case c1 of Just v -> v; Nothing -> 0) ++# c0, Nothing)
                          (True, False, True) -> (3, True, (case c2 of Just v -> v; Nothing -> 0) ++# c0, Nothing)
                          (False, True, True) -> (3, True, (case c2 of Just v -> v; Nothing -> 0) ++# (case c1 of Just v -> v; Nothing -> 0), Nothing)
                          (True, False, False) -> (3, False, 0, Just c0)
                          (False, True, False) -> (3, False, 0, c1)
                          (False, False, True) -> (3, False, 0, c2)
                          (False, False, False) -> (3, False, 0, Nothing)
                      Just coeffB ->
                        if v0
                          then (1, True, c0 ++# coeffB, Nothing)
                          else
                            if v1
                              then (2, True, (case c1 of Just v -> v; Nothing -> 0) ++# coeffB, Nothing)
                              else
                                if v2
                                  then (3, True, (case c2 of Just v -> v; Nothing -> 0) ++# coeffB, Nothing)
                                  else (3, False, 0, Just coeffB)
                  consumeN' = P.min consumeN remaining
                  nextInt = idxInt + consumeN'
                  wrap = nextInt >= 112
                  nextIdx = if wrap then fromIntegral (nextInt - 112) else fromIntegral nextInt
                  outStream =
                    AXI4Stream
                      { tdata = tdataOut,
                        tvalid = pairReady,
                        tlast = False
                      }
                  nextState =
                    if tready
                      then
                        if wrap
                          then Permute 0 state nextBuffer
                          else Squeeze nextIdx state nextBuffer
                      else Squeeze index state buffer
               in (nextState, (outStream, False))

{-# ANN
  i272o24l0
  ( Synthesize
      { t_name = "SampleNTT512_I272_O24",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "SEED" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "COEFF_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "COEFF" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "SEED_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i272o24l0 #-}
i272o24l0 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l0 clk rst en inputSig =
  withClockResetEnable clk rst en (hashWith Lookahead0 inputSig)

{-# ANN
  i272o24l1
  ( Synthesize
      { t_name = "SN512_I272_O24_L1",
        t_inputs =
          [ PortName "LOOKAHEAD",
            PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "SEED" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "COEFF_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "COEFF" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "SEED_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i272o24l1 #-}
i272o24l1 ::
  Lookahead ->
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l1 lookahead clk rst en inputSig =
  withClockResetEnable clk rst en (hashWith lookahead inputSig)

-- | Absorb 34 bytes: place message and apply padding
absorb34 :: BitVector 272 -> BitVector 1600
absorb34 = pad34Bytes . placeMsg
  where
    --  Place 34-byte message at the start of state (no XOR needed since state starts at 0)
    placeMsg :: BitVector 272 -> BitVector 1600
    placeMsg msg = (0 :: BitVector 1328) ++# msg

    --  Padding function for fixed 34-byte input + SHAKE padding.
    pad34Bytes :: BitVector 1600 -> BitVector 1600
    pad34Bytes =
      complementAt 1343 -- final pad bit (last bit of rate)
        . complementAt 272 -- DS bits in byte 34
        . complementAt 273
        . complementAt 274
        . complementAt 275
        . complementAt 276


--------------------------------------------------------------------------------

