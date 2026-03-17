module Component.SampleNTT4
  ( i272o24l4,
    i272o24l4Core,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)

data Buffer
  = Buffer0
  | Buffer1 (BitVector 12)
  | Buffer2 (BitVector 12) (BitVector 12)
  | Buffer3 (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer4 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer5 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer6 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer7 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  deriving (Show, Eq, Generic, NFDataX)

data Phase = Absorb | Permute (Index 24) | Squeeze (Index 19)
  deriving (Show, Eq, Generic, NFDataX)

data State
  = State Phase (BitVector 1600) Buffer
  deriving (Show, Eq, Generic, NFDataX)

squeezeCoeff72 :: BitVector 1600 -> Index 19 -> BitVector 72
squeezeCoeff72 state idx = case idx of
  0 -> slice (SNat @71) (SNat @0) state
  1 -> slice (SNat @143) (SNat @72) state
  2 -> slice (SNat @215) (SNat @144) state
  3 -> slice (SNat @287) (SNat @216) state
  4 -> slice (SNat @359) (SNat @288) state
  5 -> slice (SNat @431) (SNat @360) state
  6 -> slice (SNat @503) (SNat @432) state
  7 -> slice (SNat @575) (SNat @504) state
  8 -> slice (SNat @647) (SNat @576) state
  9 -> slice (SNat @719) (SNat @648) state
  10 -> slice (SNat @791) (SNat @720) state
  11 -> slice (SNat @863) (SNat @792) state
  12 -> slice (SNat @935) (SNat @864) state
  13 -> slice (SNat @1007) (SNat @936) state
  14 -> slice (SNat @1079) (SNat @1008) state
  15 -> slice (SNat @1151) (SNat @1080) state
  16 -> slice (SNat @1223) (SNat @1152) state
  17 -> slice (SNat @1295) (SNat @1224) state
  _ ->
    ((0xfff :: BitVector 12) ++# (0xfff :: BitVector 12))
      ++# slice (SNat @1343) (SNat @1296) state

{-# INLINE squeezeCoeff72 #-}

data Candidates
  = Valid0
  | Valid1 (BitVector 12)
  | Valid2 (BitVector 12) (BitVector 12)
  | Valid3 (BitVector 12) (BitVector 12) (BitVector 12)
  | Valid4 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Valid5 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Valid6 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  deriving (Show, Eq, Generic, NFDataX)

pushCandidate :: Candidates -> BitVector 12 -> Candidates
pushCandidate Valid0 c0 = Valid1 c0
pushCandidate (Valid1 c0) c1 = Valid2 c0 c1
pushCandidate (Valid2 c0 c1) c2 = Valid3 c0 c1 c2
pushCandidate (Valid3 c0 c1 c2) c3 = Valid4 c0 c1 c2 c3
pushCandidate (Valid4 c0 c1 c2 c3) c4 = Valid5 c0 c1 c2 c3 c4
pushCandidate (Valid5 c0 c1 c2 c3 c4) c5 = Valid6 c0 c1 c2 c3 c4 c5
pushCandidate valid _ = valid

screenCandidates :: BitVector 72 -> Candidates
screenCandidates chunk =
  let c0 = slice (SNat @11) (SNat @0) chunk
      c1 = slice (SNat @23) (SNat @12) chunk
      c2 = slice (SNat @35) (SNat @24) chunk
      c3 = slice (SNat @47) (SNat @36) chunk
      c4 = slice (SNat @59) (SNat @48) chunk
      c5 = slice (SNat @71) (SNat @60) chunk
      candidates =
        (c0, c0 < 3329)
          :> (c1, c1 < 3329)
          :> (c2, c2 < 3329)
          :> (c3, c3 < 3329)
          :> (c4, c4 < 3329)
          :> (c5, c5 < 3329)
          :> Nil
   in foldl
        (\valid (candidate, isValid) -> if isValid then pushCandidate valid candidate else valid)
        Valid0
        candidates

popPair :: Buffer -> (BitVector 24, Buffer)
popPair (Buffer2 a b) = (b ++# a, Buffer0)
popPair (Buffer3 a b c) = (b ++# a, Buffer1 c)
popPair (Buffer4 a b c d) = (b ++# a, Buffer2 c d)
popPair (Buffer5 a b c d e) = (b ++# a, Buffer3 c d e)
popPair (Buffer6 a b c d e f) = (b ++# a, Buffer4 c d e f)
popPair (Buffer7 a b c d e f g) = (b ++# a, Buffer5 c d e f g)
popPair _ = error "Component.SampleNTT4.popPair: buffer underflow"

step ::
  State ->
  (Bool, AXI4Stream 272) ->
  (State, (Bool, AXI4Stream 24))
step (State phase state buffer) (tready, AXI4Stream inputMsg msgValid _) = case phase of
  Absorb ->
    if msgValid
      then (State (Permute 0) (absorb34 inputMsg) Buffer0, (False, idleAXI4Stream))
      else (State Absorb state buffer, (True, idleAXI4Stream))
  Permute counter ->
    let state' = Permutation.keccakF1600 counter state
     in if counter == maxBound
          then (State (Squeeze 0) state' buffer, (False, idleAXI4Stream))
          else (State (Permute (counter + 1)) state' buffer, (False, idleAXI4Stream))
  Squeeze counter ->
    let chunk = squeezeCoeff72 state counter
        nextPhase = if counter == maxBound then Permute 0 else Squeeze (counter + 1)
     in case buffer of
          Buffer0 -> case screenCandidates chunk of
            Valid0 -> (State nextPhase state Buffer0, (False, idleAXI4Stream))
            Valid1 c0 -> (State nextPhase state (Buffer1 c0), (False, idleAXI4Stream))
            Valid2 c0 c1 ->
              if tready
                then (State nextPhase state Buffer0, (False, validBeat (c1 ++# c0) False))
                else (State nextPhase state (Buffer2 c0 c1), (False, idleAXI4Stream))
            Valid3 c0 c1 c2 ->
              if tready
                then (State nextPhase state (Buffer1 c2), (False, validBeat (c1 ++# c0) False))
                else (State nextPhase state (Buffer3 c0 c1 c2), (False, idleAXI4Stream))
            Valid4 c0 c1 c2 c3 ->
              if tready
                then (State nextPhase state (Buffer2 c2 c3), (False, validBeat (c1 ++# c0) False))
                else (State nextPhase state (Buffer4 c0 c1 c2 c3), (False, idleAXI4Stream))
            Valid5 c0 c1 c2 c3 c4 ->
              if tready
                then (State nextPhase state (Buffer3 c2 c3 c4), (False, validBeat (c1 ++# c0) False))
                else (State nextPhase state (Buffer5 c0 c1 c2 c3 c4), (False, idleAXI4Stream))
            Valid6 c0 c1 c2 c3 c4 c5 ->
              if tready
                then (State nextPhase state (Buffer4 c2 c3 c4 c5), (False, validBeat (c1 ++# c0) False))
                else (State nextPhase state (Buffer6 c0 c1 c2 c3 c4 c5), (False, idleAXI4Stream))
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
            Valid5 c0 c1 c2 c3 c4 ->
              if tready
                then (State nextPhase state (Buffer4 c1 c2 c3 c4), (False, validBeat (c0 ++# b0) False))
                else (State nextPhase state (Buffer6 b0 c0 c1 c2 c3 c4), (False, idleAXI4Stream))
            Valid6 c0 c1 c2 c3 c4 c5 ->
              if tready
                then (State nextPhase state (Buffer5 c1 c2 c3 c4 c5), (False, validBeat (c0 ++# b0) False))
                else (State nextPhase state (Buffer7 b0 c0 c1 c2 c3 c4 c5), (False, idleAXI4Stream))
          _ ->
            let (pair, buffer') = popPair buffer
             in if tready
                  then (State (Squeeze counter) state buffer', (False, validBeat pair False))
                  else (State (Squeeze counter) state buffer, (False, validBeat pair False))

i272o24l4Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 24
i272o24l4Core (coeffReady, seedStream) =
  mealyB step (State Absorb 0 Buffer0) (coeffReady, seedStream)

{-# ANN
  i272o24l4
  ( Synthesize
      { t_name = "dut",
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
{-# NOINLINE i272o24l4 #-}
i272o24l4 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l4 = toDUT i272o24l4Core

absorb34 :: BitVector 272 -> BitVector 1600
absorb34 = pad34Bytes . placeMsg
  where
    placeMsg :: BitVector 272 -> BitVector 1600
    placeMsg msg = (0 :: BitVector 1328) ++# msg

    pad34Bytes :: BitVector 1600 -> BitVector 1600
    pad34Bytes =
      complementAt 1343
        . complementAt 272
        . complementAt 273
        . complementAt 274
        . complementAt 275
        . complementAt 276
