{-# LANGUAGE TemplateHaskell #-}

module Component.SampleNTT6
  ( i272o24l6,
    i272o24l6Core,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Permutation qualified
import Sponge.NonPipelined (complementAt)
import TH (mkRead)

data Buffer
  = Buffer0
  | Buffer1 (BitVector 12)
  | Buffer2 (BitVector 12) (BitVector 12)
  | Buffer3 (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer4 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer5 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer6 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer7 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer8 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer9 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  deriving (Show, Eq, Generic, NFDataX)

data Phase = Absorb | Permute (Index 24) | Squeeze (Index 14)
  deriving (Show, Eq, Generic, NFDataX)

data State
  = State Phase (BitVector 1600) Buffer
  deriving (Show, Eq, Generic, NFDataX)

$(mkRead "squeezeCoeff96" 1600 [(i, i * 96, 96) | i <- [0 .. 13]])

{-# INLINE squeezeCoeff96 #-}

data Candidates
  = Valid0
  | Valid1 (BitVector 12)
  | Valid2 (BitVector 12) (BitVector 12)
  | Valid3 (BitVector 12) (BitVector 12) (BitVector 12)
  | Valid4 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Valid5 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Valid6 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Valid7 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Valid8 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  deriving (Show, Eq, Generic, NFDataX)

pushCandidate :: Candidates -> BitVector 12 -> Candidates
pushCandidate Valid0 c0 = Valid1 c0
pushCandidate (Valid1 c0) c1 = Valid2 c0 c1
pushCandidate (Valid2 c0 c1) c2 = Valid3 c0 c1 c2
pushCandidate (Valid3 c0 c1 c2) c3 = Valid4 c0 c1 c2 c3
pushCandidate (Valid4 c0 c1 c2 c3) c4 = Valid5 c0 c1 c2 c3 c4
pushCandidate (Valid5 c0 c1 c2 c3 c4) c5 = Valid6 c0 c1 c2 c3 c4 c5
pushCandidate (Valid6 c0 c1 c2 c3 c4 c5) c6 = Valid7 c0 c1 c2 c3 c4 c5 c6
pushCandidate (Valid7 c0 c1 c2 c3 c4 c5 c6) c7 = Valid8 c0 c1 c2 c3 c4 c5 c6 c7
pushCandidate valid _ = valid

screenCandidates :: BitVector 96 -> Candidates
screenCandidates chunk =
  let c0 = slice (SNat @11) (SNat @0) chunk
      c1 = slice (SNat @23) (SNat @12) chunk
      c2 = slice (SNat @35) (SNat @24) chunk
      c3 = slice (SNat @47) (SNat @36) chunk
      c4 = slice (SNat @59) (SNat @48) chunk
      c5 = slice (SNat @71) (SNat @60) chunk
      c6 = slice (SNat @83) (SNat @72) chunk
      c7 = slice (SNat @95) (SNat @84) chunk
      candidates =
        (c0, c0 < 3329)
          :> (c1, c1 < 3329)
          :> (c2, c2 < 3329)
          :> (c3, c3 < 3329)
          :> (c4, c4 < 3329)
          :> (c5, c5 < 3329)
          :> (c6, c6 < 3329)
          :> (c7, c7 < 3329)
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
popPair (Buffer8 a b c d e f g h) = (b ++# a, Buffer6 c d e f g h)
popPair (Buffer9 a b c d e f g h i) = (b ++# a, Buffer7 c d e f g h i)
popPair _ = error "Component.SampleNTT6.popPair: buffer underflow"

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
    let chunk = squeezeCoeff96 state counter
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
            Valid7 c0 c1 c2 c3 c4 c5 c6 ->
              if tready
                then (State nextPhase state (Buffer5 c2 c3 c4 c5 c6), (False, validBeat (c1 ++# c0) False))
                else (State nextPhase state (Buffer7 c0 c1 c2 c3 c4 c5 c6), (False, idleAXI4Stream))
            Valid8 c0 c1 c2 c3 c4 c5 c6 c7 ->
              if tready
                then (State nextPhase state (Buffer6 c2 c3 c4 c5 c6 c7), (False, validBeat (c1 ++# c0) False))
                else (State nextPhase state (Buffer8 c0 c1 c2 c3 c4 c5 c6 c7), (False, idleAXI4Stream))
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
            Valid7 c0 c1 c2 c3 c4 c5 c6 ->
              if tready
                then (State nextPhase state (Buffer6 c1 c2 c3 c4 c5 c6), (False, validBeat (c0 ++# b0) False))
                else (State nextPhase state (Buffer8 b0 c0 c1 c2 c3 c4 c5 c6), (False, idleAXI4Stream))
            Valid8 c0 c1 c2 c3 c4 c5 c6 c7 ->
              if tready
                then (State nextPhase state (Buffer7 c1 c2 c3 c4 c5 c6 c7), (False, validBeat (c0 ++# b0) False))
                else (State nextPhase state (Buffer9 b0 c0 c1 c2 c3 c4 c5 c6 c7), (False, idleAXI4Stream))
          _ ->
            let (pair, buffer') = popPair buffer
             in if tready
                  then (State (Squeeze counter) state buffer', (False, validBeat pair False))
                  else (State (Squeeze counter) state buffer, (False, validBeat pair False))

stepControl ::
  Phase ->
  BitVector 1600 ->
  Buffer ->
  Bool ->
  AXI4Stream 272 ->
  (Phase, Buffer, (Bool, AXI4Stream 24))
stepControl phase state buffer tready inStream =
  let (State phase' _stateIgnored buffer', out) = step (State phase state buffer) (tready, inStream)
   in (phase', buffer', out)

stepState ::
  Phase ->
  BitVector 1600 ->
  AXI4Stream 272 ->
  BitVector 1600
stepState phase state (AXI4Stream inputMsg msgValid _) = case phase of
  Absorb ->
    if msgValid
      then absorb34 inputMsg
      else state
  Permute counter -> Permutation.keccakF1600 counter state
  Squeeze _ -> state

i272o24l6Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 24
i272o24l6Core (coeffReady, seedStream) =
  (inReady, outStream)
  where
    phase = register Absorb phase'
    keccakState = register 0 keccakState'
    buffer = register Buffer0 buffer'

    (phase', buffer', outSig) =
      unbundle $
        stepControl
          <$> phase
          <*> keccakState
          <*> buffer
          <*> coeffReady
          <*> seedStream

    keccakState' = stepState <$> phase <*> keccakState <*> seedStream

    (inReady, outStream) = unbundle outSig

{-# ANN
  i272o24l6
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
{-# NOINLINE i272o24l6 #-}
i272o24l6 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l6 = toDUT i272o24l6Core

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
