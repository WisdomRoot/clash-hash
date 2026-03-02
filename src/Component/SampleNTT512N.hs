{-# LANGUAGE TemplateHaskell #-}

module Component.SampleNTT512N
  ( i272o24l2,
    i272o24l2Top,
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
  (Bool, AXI4Stream 272) ->
  (State, (Bool, AXI4Stream 24))
step (State phase state buffer) (tready, AXI4Stream inputMsg msgValid _) = case phase of
  Absorb ->
    -- TREADY is True, waiting for TVALID
    if msgValid
      then (State (Permute 0) (absorb34 inputMsg) Buffer0, (False, idleAXI4Stream))
      else (State Absorb state buffer, (True, idleAXI4Stream))
  Permute counter ->
    let state' = Permutation.keccakF1600 counter state
     in if counter == maxBound
          then (State (Squeeze 0) state' buffer, (False, idleAXI4Stream))
          else (State (Permute (counter + 1)) state' buffer, (False, idleAXI4Stream))
  Squeeze counter ->
    let chunk = squeezeCoeff48 state counter
        nextPhase = if counter == maxBound then Permute 0 else Squeeze (counter + 1)
     in case buffer of
          Buffer0 -> case screenCandidates chunk of
            Valid0 -> (State nextPhase state buffer, (False, idleAXI4Stream))
            Valid1 c0 -> (State nextPhase state (Buffer1 c0), (False, idleAXI4Stream))
            Valid2 c0 c1 -> 
              if tready 
                then (State nextPhase state buffer, (False, validBeat (c1 ++# c0) False))
                else (State nextPhase state (Buffer2 c0 c1), (False, idleAXI4Stream))
            Valid3 c0 c1 c2 -> 
              if tready 
                then (State nextPhase state (Buffer1 c2), (False, validBeat (c1 ++# c0) False))
                else (State nextPhase state (Buffer3 c0 c1 c2), (False, idleAXI4Stream))
            Valid4 c0 c1 c2 c3 -> 
              if tready
                then (State nextPhase state (Buffer2 c2 c3), (False, validBeat (c1 ++# c0) False))
                else (State nextPhase state (Buffer4 c0 c1 c2 c3), (False, idleAXI4Stream))
          Buffer1 b0 -> case screenCandidates chunk of
            Valid0 -> (State nextPhase state (Buffer1 b0), (False, idleAXI4Stream))
            Valid1 c0 -> 
              if tready
                then (State nextPhase state Buffer0, (False, validBeat (c0 ++# b0) False))
                else (State nextPhase state (Buffer2 b0 c0), (False, idleAXI4Stream))
            Valid2 c0 c1 -> 
              if tready
                then (State nextPhase state (Buffer1 c1), (False, validBeat (c0 ++# b0) False))
                else (State nextPhase state (Buffer3 b0 c0 c1), (False, idleAXI4Stream))
            Valid3 c0 c1 c2 -> 
               if tready
                then (State nextPhase state (Buffer2 c1 c2), (False, validBeat (c0 ++# b0) False))
                else (State nextPhase state (Buffer4 b0 c0 c1 c2), (False, idleAXI4Stream))
            Valid4 c0 c1 c2 c3 -> 
               if tready
                then (State nextPhase state (Buffer3 c1 c2 c3), (False, validBeat (c0 ++# b0) False))
                else (State nextPhase state (Buffer5 b0 c0 c1 c2 c3), (False, idleAXI4Stream))
          Buffer2 b0 b1 -> (State (Squeeze counter) state Buffer0, (False, validBeat (b1 ++# b0) False))
          Buffer3 b0 b1 b2 -> (State (Squeeze counter) state (Buffer1 b2), (False, validBeat (b1 ++# b0) False))
          Buffer4 b0 b1 b2 b3 -> (State (Squeeze counter) state (Buffer2 b2 b3), (False, validBeat (b1 ++# b0) False))
          Buffer5 b0 b1 b2 b3 b4 -> (State (Squeeze counter) state (Buffer3 b2 b3 b4), (False, validBeat (b1 ++# b0) False))

i272o24l2 ::
  HiddenClockResetEnable dom =>
  Stage dom 272 24
i272o24l2 (coeffReady, seedStream) =
  mealyB step (State Absorb 0 Buffer0) (coeffReady, seedStream)

{-# ANN
  i272o24l2Top
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
{-# NOINLINE i272o24l2Top #-}
i272o24l2Top ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l2Top = stageTop i272o24l2

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
