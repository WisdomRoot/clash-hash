{-# LANGUAGE TemplateHaskell #-}

module Component.SampleNTT512N
  ( i272o24l2,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)
import TH (mkRead)

-- | Collected coefficients that are pending output
data Buffer = Buffer0 | Buffer1 (BitVector 12) | Buffer2 (BitVector 12) (BitVector 12) | Buffer3 (BitVector 12) (BitVector 12) (BitVector 12) | Buffer4 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) | Buffer5 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  deriving (Show, Eq, Generic, NFDataX)

data Phase = Absorb | Permute (Index 24) | Squeeze (Index 28)
  deriving (Show, Eq, Generic, NFDataX)

data State
  = State Phase (BitVector 1600) Buffer
  deriving (Show, Eq, Generic, NFDataX)

-- | Extract 48-bit coefficient pair from state (pattern matched on all 56 indices)
$( mkRead
     "squeezeCoeff48"
     1600
     [(i, i * 48, 48) | i <- [0 .. 27]]
 )

{-# INLINE squeezeCoeff48 #-}

-- -- | Clean hashWith function for fixed 34-byte input with AXI4-Stream input handshaking
-- hashWith ::
--   forall dom.
--   (HiddenClockResetEnable dom) =>
--   Lookahead ->
--   Signal dom (AXI4Stream 272, Bool) ->
--   Signal dom (AXI4Stream 24, Bool)
-- hashWith lookahead = mealy step (Idle :: State)
--   where
--     step ::
--       State ->
--       (AXI4Stream 272, Bool) ->
--       (State, (AXI4Stream 24, Bool))
--     step st (AXI4Stream inputMsg msgValid _, tready) =
--       case st of
--         Idle ->
--           -- SEED_TREADY is True, waiting for SEED_TVALID
--           if msgValid
--             then (Permute 0 (absorb34 inputMsg) Nothing, (idleAXI4Stream, False))
--             else (Idle, (idleAXI4Stream, True))
--         Permute roundIdx state buffer ->
--           let state' = Permutation.keccakF1600 roundIdx state
--            in if roundIdx == maxBound
--                 then (Squeeze 0 state' buffer, (idleAXI4Stream, False))
--                 else (Permute (roundIdx + 1) state' buffer, (idleAXI4Stream, False))
--         Squeeze index state buffer ->
--           case lookahead of
--             Lookahead0 ->
--               let idxInt = fromEnum index
--                   pairIdx = fromIntegral (idxInt `div` 2) :: Index 56
--                   pair = squeezeCoeff24 state pairIdx
--                   c0 = slice (SNat @11) (SNat @0) pair
--                   c1 = slice (SNat @23) (SNat @12) pair
--                   v0 = (unpack c0 :: Unsigned 12) < (3329 :: Unsigned 12)
--                   v1 = (unpack c1 :: Unsigned 12) < (3329 :: Unsigned 12)
--                   wrap = idxInt >= 110
--                   nextIdx = if wrap then 0 else fromIntegral (idxInt + 2)
--                   (pairReady, tdataOut, nextBuffer) =
--                     case (buffer, v0, v1) of
--                       (Just b, True, True) -> (True, c0 ++# b, Just c1)
--                       (Just b, True, False) -> (True, c0 ++# b, Nothing)
--                       (Just b, False, True) -> (True, c1 ++# b, Nothing)
--                       (Just b, False, False) -> (False, 0, Just b)
--                       (Nothing, True, True) -> (True, c1 ++# c0, Nothing)
--                       (Nothing, True, False) -> (False, 0, Just c0)
--                       (Nothing, False, True) -> (False, 0, Just c1)
--                       (Nothing, False, False) -> (False, 0, Nothing)
--                   outStream =
--                     AXI4Stream
--                       { tdata = tdataOut,
--                         tvalid = pairReady,
--                         tlast = False
--                       }
--                   nextState =
--                     if tready
--                       then
--                         if wrap
--                           then Permute 0 state nextBuffer
--                           else Squeeze nextIdx state nextBuffer
--                       else Squeeze index state buffer
--                in (nextState, (outStream, False))
--             Lookahead1 ->
--               let idxInt = fromEnum index
--                   remaining = 112 - idxInt
--                   c0 = squeezeCoeff12 state index
--                   c1 =
--                     if remaining P.> 1
--                       then Just (squeezeCoeff12 state (fromIntegral (idxInt + 1)))
--                       else Nothing
--                   c2 =
--                     if remaining P.> 2
--                       then Just (squeezeCoeff12 state (fromIntegral (idxInt + 2)))
--                       else Nothing
--                   v0 = (unpack c0 :: Unsigned 12) < (3329 :: Unsigned 12)
--                   v1 = case c1 of
--                     Just v -> (unpack v :: Unsigned 12) < (3329 :: Unsigned 12)
--                     Nothing -> False
--                   v2 = case c2 of
--                     Just v -> (unpack v :: Unsigned 12) < (3329 :: Unsigned 12)
--                     Nothing -> False
--                   (consumeN, pairReady, tdataOut, nextBuffer) =
--                     case buffer of
--                       Nothing ->
--                         case (v0, v1, v2) of
--                           (True, True, _) -> (2, True, (case c1 of Just v -> v; Nothing -> 0) ++# c0, Nothing)
--                           (True, False, True) -> (3, True, (case c2 of Just v -> v; Nothing -> 0) ++# c0, Nothing)
--                           (False, True, True) -> (3, True, (case c2 of Just v -> v; Nothing -> 0) ++# (case c1 of Just v -> v; Nothing -> 0), Nothing)
--                           (True, False, False) -> (3, False, 0, Just c0)
--                           (False, True, False) -> (3, False, 0, c1)
--                           (False, False, True) -> (3, False, 0, c2)
--                           (False, False, False) -> (3, False, 0, Nothing)
--                       Just coeffB ->
--                         if v0
--                           then (1, True, c0 ++# coeffB, Nothing)
--                           else
--                             if v1
--                               then (2, True, (case c1 of Just v -> v; Nothing -> 0) ++# coeffB, Nothing)
--                               else
--                                 if v2
--                                   then (3, True, (case c2 of Just v -> v; Nothing -> 0) ++# coeffB, Nothing)
--                                   else (3, False, 0, Just coeffB)
--                   consumeN' = P.min consumeN remaining
--                   nextInt = idxInt + consumeN'
--                   wrap = nextInt >= 112
--                   nextIdx = if wrap then fromIntegral (nextInt - 112) else fromIntegral nextInt
--                   outStream =
--                     AXI4Stream
--                       { tdata = tdataOut,
--                         tvalid = pairReady,
--                         tlast = False
--                       }
--                   nextState =
--                     if tready
--                       then
--                         if wrap
--                           then Permute 0 state nextBuffer
--                           else Squeeze nextIdx state nextBuffer
--                       else Squeeze index state buffer
--                in (nextState, (outStream, False))

data Candidates
  = Valid0 -- no valid coeffs
  | Valid1 (BitVector 12) -- 1 valid coeff
  | Valid2 (BitVector 12) (BitVector 12) -- 2 valid coeffs
  | Valid3 (BitVector 12) (BitVector 12) (BitVector 12) -- 3 valid coeffs
  | Valid4 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) -- 4 valid coeffs
  deriving (Show, Eq, Generic, NFDataX)

screenCandidates :: BitVector 48 -> Candidates
screenCandidates chunk =
  let c0 = slice (SNat @11) (SNat @0) chunk
      c1 = slice (SNat @23) (SNat @12) chunk
      c2 = slice (SNat @35) (SNat @24) chunk
      c3 = slice (SNat @47) (SNat @36) chunk
   in case (c0 < 3329, c1 < 3329, c2 < 3329, c3 < 3329) of
        (False, False, False, False) -> Valid0
        (True, False, False, False) -> Valid1 c0
        (False, True, False, False) -> Valid1 c1
        (False, False, True, False) -> Valid1 c2
        (False, False, False, True) -> Valid1 c3
        (True, True, False, False) -> Valid2 c0 c1
        (True, False, True, False) -> Valid2 c0 c2
        (True, False, False, True) -> Valid2 c0 c3
        (False, True, True, False) -> Valid2 c1 c2
        (False, True, False, True) -> Valid2 c1 c3
        (False, False, True, True) -> Valid2 c2 c3
        (True, True, True, False) -> Valid3 c0 c1 c2
        (True, True, False, True) -> Valid3 c0 c1 c3
        (True, False, True, True) -> Valid3 c0 c2 c3
        (False, True, True, True) -> Valid3 c1 c2 c3
        (True, True, True, True) -> Valid4 c0 c1 c2 c3

step ::
  State ->
  (AXI4Stream 272, Bool) ->
  (State, (AXI4Stream 24, Bool))
step (State phase state buffer) (AXI4Stream inputMsg msgValid _, tready) = case phase of
  Absorb ->
    -- TREADY is True, waiting for TVALID
    if msgValid
      then (State (Permute 0) (absorb34 inputMsg) Buffer0, (idleAXI4Stream, False))
      else (State Absorb state buffer, (idleAXI4Stream, True))
  Permute counter ->
    let state' = Permutation.keccakF1600 counter state
     in if counter == maxBound
          then (State (Squeeze 0) state' buffer, (idleAXI4Stream, False))
          else (State (Permute (counter + 1)) state' buffer, (idleAXI4Stream, False))
  Squeeze counter ->
    let chunk = squeezeCoeff48 state counter
     in case buffer of
          Buffer0 -> case screenCandidates chunk of
            Valid0 -> (State (Squeeze (counter + 1)) state buffer, (idleAXI4Stream, False))
            Valid1 c0 -> (State (Squeeze (counter + 1)) state (Buffer1 c0), (idleAXI4Stream, False))
            Valid2 c0 c1 -> 
              if tready 
                then (State (Squeeze (counter + 1)) state buffer, (validBeat (c0 ++# c1) False, False))
                else (State (Squeeze (counter + 1)) state (Buffer2 c0 c1), (idleAXI4Stream, False))
            Valid3 c0 c1 c2 -> 
              if tready 
                then (State (Squeeze (counter + 1)) state (Buffer1 c2), (validBeat (c0 ++# c1) False, False))
                else (State (Squeeze (counter + 1)) state (Buffer3 c0 c1 c2), (idleAXI4Stream, False))
            Valid4 c0 c1 c2 c3 -> 
              if tready
                then (State (Squeeze (counter + 1)) state (Buffer2 c2 c3), (validBeat (c0 ++# c1) False, False))
                else (State (Squeeze counter) state buffer, (idleAXI4Stream, False))
          Buffer1 b0 -> case screenCandidates chunk of
            Valid0 -> (State (Squeeze (counter + 1)) state (Buffer1 b0), (idleAXI4Stream, False))
            Valid1 c0 -> 
              if tready
                then (State (Squeeze (counter + 1)) state Buffer0, (validBeat (b0 ++# c0) False, False))
                else (State (Squeeze (counter + 1)) state (Buffer2 b0 c0), (idleAXI4Stream, False))
            Valid2 c0 c1 -> 
              if tready
                then (State (Squeeze (counter + 1)) state (Buffer1 c1), (validBeat (b0 ++# c0) False, False))
                else (State (Squeeze (counter + 1)) state (Buffer3 b0 c0 c1), (idleAXI4Stream, False))
            Valid3 c0 c1 c2 -> 
               if tready
                then (State (Squeeze (counter + 1)) state (Buffer2 c1 c2), (validBeat (b0 ++# c0) False, False))
                else (State (Squeeze (counter + 1)) state (Buffer4 b0 c0 c1 c2), (idleAXI4Stream, False))
            Valid4 c0 c1 c2 c3 -> 
               if tready
                then (State (Squeeze (counter + 1)) state (Buffer3 c1 c2 c3), (validBeat (b0 ++# c0) False, False))
                else (State (Squeeze (counter + 1)) state (Buffer5 b0 c0 c1 c2 c3), (idleAXI4Stream, False))
          Buffer2 b0 b1 -> (State (Squeeze counter) state Buffer0, (validBeat (b0 ++# b1) False, False))
          Buffer3 b0 b1 b2 -> (State (Squeeze counter) state (Buffer1 b2), (validBeat (b0 ++# b1) False, False))
          Buffer4 b0 b1 b2 b3 -> (State (Squeeze counter) state (Buffer2 b2 b3), (validBeat (b0 ++# b1) False, False))
          Buffer5 b0 b1 b2 b3 b4 -> (State (Squeeze counter) state (Buffer3 b2 b3 b4), (validBeat (b0 ++# b1) False, False))

-- case screenCandidates chunk of
--     Valid0 -> (State (Squeeze (counter + 1)) state buffer, (idleAXI4Stream, False))
--     Valid1 c0 -> let outStream = validBeat c0 False in (State (Squeeze (counter + 1)) state buffer, (outStream, False))
--     Valid2 c0 c1 -> let outStream = validBeat (c1 ++# c0) False in (State (Squeeze (counter + 1)) state buffer, (outStream, False))
--     Valid3 c0 c1 c2 -> let outStream = validBeat (c2 ++# c0) False in (State (Squeeze (counter + 1)) state buffer, (outStream, False))
--     Valid4 c0 c1 c2 c3 -> let outStream = validBeat (c3 ++# c0) False in (State (Squeeze (counter + 1)) state buffer, (outStream, False))

--  in if tready
--       then State (Squeeze counter) state buffer
--         -- if wrap
--         --   then Permute 0 state nextBuffer
--         --   else Squeeze nextIdx state nextBuffer
--       else State (Squeeze counter) state buffer -- hold
-- case screenCandidates chunk of
--     Candidates0 -> (State (Squeeze (counter + 1)) state buffer, (idleAXI4Stream, False))
--     Candidates1 c0 -> let outStream = validBeat c0 False in (State (Squeeze (counter + 1)) state buffer, (outStream, False))
--     Candidates2 c0 c1 -> let outStream = validBeat (c1 ++# c0) False in (State (Squeeze (counter + 1)) state buffer, (outStream, False))
--     Candidates3 c0 c1 c2 -> let outStream = validBeat (c2 ++# c0) False in (State (Squeeze (counter + 1)) state buffer, (outStream, False))
--     Candidates4 c0 c1 c2 c3 -> let outStream = validBeat (c3 ++# c0) False in (State (Squeeze (counter + 1)) state buffer, (outStream, False))

--     v0 = (unpack c0 :: Unsigned 12) < (3329 :: Unsigned 12)
--     v1 = (unpack c1 :: Unsigned 12) < (3329 :: Unsigned 12)
--     wrap = idxInt >= 110
--     nextIdx = if wrap then 0 else fromIntegral (idxInt + 2)
--     (pairReady, tdataOut, nextBuffer) =
--       case (buffer, v0, v1) of
--         (Just b, True, True) -> (True, c0 ++# b, Just c1)
--         (Just b, True, False) -> (True, c0 ++# b, Nothing)
--         (Just b, False, True) -> (True, c1 ++# b, Nothing)
--         (Just b, False, False) -> (False, 0, Just b)
--         (Nothing, True, True) -> (True, c1 ++# c0, Nothing)
--         (Nothing, True, False) -> (False, 0, Just c0)
--         (Nothing, False, True) -> (False, 0, Just c1)
--         (Nothing, False, False) -> (False, 0, Nothing)
--     outStream =
--       AXI4Stream
--         { tdata = tdataOut,
--           tvalid = pairReady,
--           tlast = False
--         }
--     nextState =
--       if tready
--         then
--           if wrap
--             then Permute 0 state nextBuffer
--             else Squeeze nextIdx state nextBuffer
--         else Squeeze index state buffer
--  in (nextState, (outStream, False))

{-# ANN
  i272o24l2
  ( Synthesize
      { t_name = "SN512_I272_O24_L2",
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
{-# NOINLINE i272o24l2 #-}
i272o24l2 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l2 clk rst en inputSig = withClockResetEnable clk rst en (mealy step (State Absorb 0 Buffer0) inputSig)

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
