{-# LANGUAGE TemplateHaskell #-}

module Component.SampleNTT6
  ( i272o24l6,
    i272o24l6Core,
    i272o24l6Merged,
    i272o24l6MergedCore,
    i272o24l6x2,
    i272o24l6x2Core,
    i272o24l6x3,
    i272o24l6x3Core,
    i272o24l6x4,
    i272o24l6x4Core,
    i272o24l6x6,
    i272o24l6x6Core,
    i272o24l6x8,
    i272o24l6x8Core,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Component.SampleNTT.Common (absorb34, screenCoeff96)
import Permutation qualified
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
  foldl
    (\valid (isValid, candidate) -> if isValid then pushCandidate valid candidate else valid)
    Valid0
    (screenCoeff96 chunk)

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
stepControl phase state buffer tready (AXI4Stream inputMsg msgValid _) = case phase of
  Absorb ->
    if msgValid
      then (Permute 0, Buffer0, (False, idleAXI4Stream))
      else (Absorb, buffer, (True, idleAXI4Stream))
  Permute counter ->
    if counter == maxBound
      then (Squeeze 0, buffer, (False, idleAXI4Stream))
      else (Permute (counter + 1), buffer, (False, idleAXI4Stream))
  Squeeze counter ->
    let chunk = squeezeCoeff96 state counter
        nextPhase = if counter == maxBound then Permute 0 else Squeeze (counter + 1)
     in case buffer of
          Buffer0 -> case screenCandidates chunk of
            Valid0 -> (nextPhase, Buffer0, (False, idleAXI4Stream))
            Valid1 c0 -> (nextPhase, Buffer1 c0, (False, idleAXI4Stream))
            Valid2 c0 c1 ->
              if tready
                then (nextPhase, Buffer0, (False, validBeat (c1 ++# c0) False))
                else (nextPhase, Buffer2 c0 c1, (False, idleAXI4Stream))
            Valid3 c0 c1 c2 ->
              if tready
                then (nextPhase, Buffer1 c2, (False, validBeat (c1 ++# c0) False))
                else (nextPhase, Buffer3 c0 c1 c2, (False, idleAXI4Stream))
            Valid4 c0 c1 c2 c3 ->
              if tready
                then (nextPhase, Buffer2 c2 c3, (False, validBeat (c1 ++# c0) False))
                else (nextPhase, Buffer4 c0 c1 c2 c3, (False, idleAXI4Stream))
            Valid5 c0 c1 c2 c3 c4 ->
              if tready
                then (nextPhase, Buffer3 c2 c3 c4, (False, validBeat (c1 ++# c0) False))
                else (nextPhase, Buffer5 c0 c1 c2 c3 c4, (False, idleAXI4Stream))
            Valid6 c0 c1 c2 c3 c4 c5 ->
              if tready
                then (nextPhase, Buffer4 c2 c3 c4 c5, (False, validBeat (c1 ++# c0) False))
                else (nextPhase, Buffer6 c0 c1 c2 c3 c4 c5, (False, idleAXI4Stream))
            Valid7 c0 c1 c2 c3 c4 c5 c6 ->
              if tready
                then (nextPhase, Buffer5 c2 c3 c4 c5 c6, (False, validBeat (c1 ++# c0) False))
                else (nextPhase, Buffer7 c0 c1 c2 c3 c4 c5 c6, (False, idleAXI4Stream))
            Valid8 c0 c1 c2 c3 c4 c5 c6 c7 ->
              if tready
                then (nextPhase, Buffer6 c2 c3 c4 c5 c6 c7, (False, validBeat (c1 ++# c0) False))
                else (nextPhase, Buffer8 c0 c1 c2 c3 c4 c5 c6 c7, (False, idleAXI4Stream))
          Buffer1 b0 -> case screenCandidates chunk of
            Valid0 -> (nextPhase, Buffer1 b0, (False, idleAXI4Stream))
            Valid1 c0 ->
              if tready
                then (nextPhase, Buffer0, (False, validBeat (c0 ++# b0) False))
                else (nextPhase, Buffer2 b0 c0, (False, idleAXI4Stream))
            Valid2 c0 c1 ->
              if tready
                then (nextPhase, Buffer1 c1, (False, validBeat (c0 ++# b0) False))
                else (nextPhase, Buffer3 b0 c0 c1, (False, idleAXI4Stream))
            Valid3 c0 c1 c2 ->
              if tready
                then (nextPhase, Buffer2 c1 c2, (False, validBeat (c0 ++# b0) False))
                else (nextPhase, Buffer4 b0 c0 c1 c2, (False, idleAXI4Stream))
            Valid4 c0 c1 c2 c3 ->
              if tready
                then (nextPhase, Buffer3 c1 c2 c3, (False, validBeat (c0 ++# b0) False))
                else (nextPhase, Buffer5 b0 c0 c1 c2 c3, (False, idleAXI4Stream))
            Valid5 c0 c1 c2 c3 c4 ->
              if tready
                then (nextPhase, Buffer4 c1 c2 c3 c4, (False, validBeat (c0 ++# b0) False))
                else (nextPhase, Buffer6 b0 c0 c1 c2 c3 c4, (False, idleAXI4Stream))
            Valid6 c0 c1 c2 c3 c4 c5 ->
              if tready
                then (nextPhase, Buffer5 c1 c2 c3 c4 c5, (False, validBeat (c0 ++# b0) False))
                else (nextPhase, Buffer7 b0 c0 c1 c2 c3 c4 c5, (False, idleAXI4Stream))
            Valid7 c0 c1 c2 c3 c4 c5 c6 ->
              if tready
                then (nextPhase, Buffer6 c1 c2 c3 c4 c5 c6, (False, validBeat (c0 ++# b0) False))
                else (nextPhase, Buffer8 b0 c0 c1 c2 c3 c4 c5 c6, (False, idleAXI4Stream))
            Valid8 c0 c1 c2 c3 c4 c5 c6 c7 ->
              if tready
                then (nextPhase, Buffer7 c1 c2 c3 c4 c5 c6 c7, (False, validBeat (c0 ++# b0) False))
                else (nextPhase, Buffer9 b0 c0 c1 c2 c3 c4 c5 c6 c7, (False, idleAXI4Stream))
          _ ->
            let (pair, buffer') = popPair buffer
             in if tready
                  then (Squeeze counter, buffer', (False, validBeat pair False))
                  else (Squeeze counter, buffer, (False, validBeat pair False))

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

stepX2 ::
  State ->
  (Bool, AXI4Stream 272) ->
  (State, (Bool, AXI4Stream 24))
stepX2 (State phase state buffer) (tready, inputStream) = case phase of
  Permute counter ->
    let state1 = Permutation.keccakF1600 counter state
        state2 = Permutation.keccakF1600 (counter + 1) state1
     in if counter == 22
          then (State (Squeeze 0) state2 buffer, (False, idleAXI4Stream))
          else (State (Permute (counter + 2)) state2 buffer, (False, idleAXI4Stream))
  _ -> step (State phase state buffer) (tready, inputStream)

stepX3 ::
  State ->
  (Bool, AXI4Stream 272) ->
  (State, (Bool, AXI4Stream 24))
stepX3 (State phase state buffer) (tready, inputStream) = case phase of
  Permute counter ->
    let state1 = Permutation.keccakF1600 counter state
        state2 = Permutation.keccakF1600 (counter + 1) state1
        state3 = Permutation.keccakF1600 (counter + 2) state2
     in if counter == 21
          then (State (Squeeze 0) state3 buffer, (False, idleAXI4Stream))
          else (State (Permute (counter + 3)) state3 buffer, (False, idleAXI4Stream))
  _ -> step (State phase state buffer) (tready, inputStream)

stepX4 ::
  State ->
  (Bool, AXI4Stream 272) ->
  (State, (Bool, AXI4Stream 24))
stepX4 (State phase state buffer) (tready, inputStream) = case phase of
  Permute counter ->
    let state1 = Permutation.keccakF1600 counter state
        state2 = Permutation.keccakF1600 (counter + 1) state1
        state3 = Permutation.keccakF1600 (counter + 2) state2
        state4 = Permutation.keccakF1600 (counter + 3) state3
     in if counter == 20
          then (State (Squeeze 0) state4 buffer, (False, idleAXI4Stream))
          else (State (Permute (counter + 4)) state4 buffer, (False, idleAXI4Stream))
  _ -> step (State phase state buffer) (tready, inputStream)

stepX6 ::
  State ->
  (Bool, AXI4Stream 272) ->
  (State, (Bool, AXI4Stream 24))
stepX6 (State phase state buffer) (tready, inputStream) = case phase of
  Permute counter ->
    let state1 = Permutation.keccakF1600 counter state
        state2 = Permutation.keccakF1600 (counter + 1) state1
        state3 = Permutation.keccakF1600 (counter + 2) state2
        state4 = Permutation.keccakF1600 (counter + 3) state3
        state5 = Permutation.keccakF1600 (counter + 4) state4
        state6 = Permutation.keccakF1600 (counter + 5) state5
     in if counter == 18
          then (State (Squeeze 0) state6 buffer, (False, idleAXI4Stream))
          else (State (Permute (counter + 6)) state6 buffer, (False, idleAXI4Stream))
  _ -> step (State phase state buffer) (tready, inputStream)

stepX8 ::
  State ->
  (Bool, AXI4Stream 272) ->
  (State, (Bool, AXI4Stream 24))
stepX8 (State phase state buffer) (tready, inputStream) = case phase of
  Permute counter ->
    let state1 = Permutation.keccakF1600 counter state
        state2 = Permutation.keccakF1600 (counter + 1) state1
        state3 = Permutation.keccakF1600 (counter + 2) state2
        state4 = Permutation.keccakF1600 (counter + 3) state3
        state5 = Permutation.keccakF1600 (counter + 4) state4
        state6 = Permutation.keccakF1600 (counter + 5) state5
        state7 = Permutation.keccakF1600 (counter + 6) state6
        state8 = Permutation.keccakF1600 (counter + 7) state7
     in if counter == 16
          then (State (Squeeze 0) state8 buffer, (False, idleAXI4Stream))
          else (State (Permute (counter + 8)) state8 buffer, (False, idleAXI4Stream))
  _ -> step (State phase state buffer) (tready, inputStream)

stepControlX2 ::
  Phase ->
  BitVector 1600 ->
  Buffer ->
  Bool ->
  AXI4Stream 272 ->
  (Phase, Buffer, (Bool, AXI4Stream 24))
stepControlX2 = stepControl

stepStateX2 ::
  Phase ->
  BitVector 1600 ->
  AXI4Stream 272 ->
  BitVector 1600
stepStateX2 phase state inStream = case phase of
  Permute counter ->
    let state1 = Permutation.keccakF1600 counter state
        state2 = Permutation.keccakF1600 (counter + 1) state1
     in if counter == 22 then state2 else state2
  _ -> stepState phase state inStream

stepControlX3 ::
  Phase ->
  BitVector 1600 ->
  Buffer ->
  Bool ->
  AXI4Stream 272 ->
  (Phase, Buffer, (Bool, AXI4Stream 24))
stepControlX3 phase state buffer tready inStream = case phase of
  Permute counter ->
    if counter == 21
      then (Squeeze 0, buffer, (False, idleAXI4Stream))
      else (Permute (counter + 3), buffer, (False, idleAXI4Stream))
  _ -> stepControl phase state buffer tready inStream

stepStateX3 ::
  Phase ->
  BitVector 1600 ->
  AXI4Stream 272 ->
  BitVector 1600
stepStateX3 phase state inStream = case phase of
  Permute counter ->
    let state1 = Permutation.keccakF1600 counter state
        state2 = Permutation.keccakF1600 (counter + 1) state1
        state3 = Permutation.keccakF1600 (counter + 2) state2
     in state3
  _ -> stepState phase state inStream

stepControlX4 ::
  Phase ->
  BitVector 1600 ->
  Buffer ->
  Bool ->
  AXI4Stream 272 ->
  (Phase, Buffer, (Bool, AXI4Stream 24))
stepControlX4 phase state buffer tready inStream = case phase of
  Permute counter ->
    if counter == 20
      then (Squeeze 0, buffer, (False, idleAXI4Stream))
      else (Permute (counter + 4), buffer, (False, idleAXI4Stream))
  _ -> stepControl phase state buffer tready inStream

stepStateX4 ::
  Phase ->
  BitVector 1600 ->
  AXI4Stream 272 ->
  BitVector 1600
stepStateX4 phase state inStream = case phase of
  Permute counter ->
    let state1 = Permutation.keccakF1600 counter state
        state2 = Permutation.keccakF1600 (counter + 1) state1
        state3 = Permutation.keccakF1600 (counter + 2) state2
        state4 = Permutation.keccakF1600 (counter + 3) state3
     in state4
  _ -> stepState phase state inStream

stepControlX6 ::
  Phase ->
  BitVector 1600 ->
  Buffer ->
  Bool ->
  AXI4Stream 272 ->
  (Phase, Buffer, (Bool, AXI4Stream 24))
stepControlX6 phase state buffer tready inStream = case phase of
  Permute counter ->
    if counter == 18
      then (Squeeze 0, buffer, (False, idleAXI4Stream))
      else (Permute (counter + 6), buffer, (False, idleAXI4Stream))
  _ -> stepControl phase state buffer tready inStream

stepStateX6 ::
  Phase ->
  BitVector 1600 ->
  AXI4Stream 272 ->
  BitVector 1600
stepStateX6 phase state inStream = case phase of
  Permute counter ->
    let state1 = Permutation.keccakF1600 counter state
        state2 = Permutation.keccakF1600 (counter + 1) state1
        state3 = Permutation.keccakF1600 (counter + 2) state2
        state4 = Permutation.keccakF1600 (counter + 3) state3
        state5 = Permutation.keccakF1600 (counter + 4) state4
        state6 = Permutation.keccakF1600 (counter + 5) state5
     in state6
  _ -> stepState phase state inStream

stepControlX8 ::
  Phase ->
  BitVector 1600 ->
  Buffer ->
  Bool ->
  AXI4Stream 272 ->
  (Phase, Buffer, (Bool, AXI4Stream 24))
stepControlX8 phase state buffer tready inStream = case phase of
  Permute counter ->
    if counter == 16
      then (Squeeze 0, buffer, (False, idleAXI4Stream))
      else (Permute (counter + 8), buffer, (False, idleAXI4Stream))
  _ -> stepControl phase state buffer tready inStream

stepStateX8 ::
  Phase ->
  BitVector 1600 ->
  AXI4Stream 272 ->
  BitVector 1600
stepStateX8 phase state inStream = case phase of
  Permute counter ->
    let state1 = Permutation.keccakF1600 counter state
        state2 = Permutation.keccakF1600 (counter + 1) state1
        state3 = Permutation.keccakF1600 (counter + 2) state2
        state4 = Permutation.keccakF1600 (counter + 3) state3
        state5 = Permutation.keccakF1600 (counter + 4) state4
        state6 = Permutation.keccakF1600 (counter + 5) state5
        state7 = Permutation.keccakF1600 (counter + 6) state6
        state8 = Permutation.keccakF1600 (counter + 7) state7
     in state8
  _ -> stepState phase state inStream

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

i272o24l6MergedCore ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 24
i272o24l6MergedCore (coeffReady, seedStream) =
  mealyB step (State Absorb 0 Buffer0) (coeffReady, seedStream)

i272o24l6x2Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 24
i272o24l6x2Core (coeffReady, seedStream) =
  (inReady, outStream)
  where
    phase = register Absorb phase'
    keccakState = register 0 keccakState'
    buffer = register Buffer0 buffer'

    (phase', buffer', outSig) =
      unbundle $
        stepControlX2
          <$> phase
          <*> keccakState
          <*> buffer
          <*> coeffReady
          <*> seedStream

    keccakState' = stepStateX2 <$> phase <*> keccakState <*> seedStream
    (inReady, outStream) = unbundle outSig

i272o24l6x3Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 24
i272o24l6x3Core (coeffReady, seedStream) =
  (inReady, outStream)
  where
    phase = register Absorb phase'
    keccakState = register 0 keccakState'
    buffer = register Buffer0 buffer'

    (phase', buffer', outSig) =
      unbundle $
        stepControlX3
          <$> phase
          <*> keccakState
          <*> buffer
          <*> coeffReady
          <*> seedStream

    keccakState' = stepStateX3 <$> phase <*> keccakState <*> seedStream
    (inReady, outStream) = unbundle outSig

i272o24l6x4Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 24
i272o24l6x4Core (coeffReady, seedStream) =
  (inReady, outStream)
  where
    phase = register Absorb phase'
    keccakState = register 0 keccakState'
    buffer = register Buffer0 buffer'

    (phase', buffer', outSig) =
      unbundle $
        stepControlX4
          <$> phase
          <*> keccakState
          <*> buffer
          <*> coeffReady
          <*> seedStream

    keccakState' = stepStateX4 <$> phase <*> keccakState <*> seedStream
    (inReady, outStream) = unbundle outSig

i272o24l6x6Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 24
i272o24l6x6Core (coeffReady, seedStream) =
  (inReady, outStream)
  where
    phase = register Absorb phase'
    keccakState = register 0 keccakState'
    buffer = register Buffer0 buffer'

    (phase', buffer', outSig) =
      unbundle $
        stepControlX6
          <$> phase
          <*> keccakState
          <*> buffer
          <*> coeffReady
          <*> seedStream

    keccakState' = stepStateX6 <$> phase <*> keccakState <*> seedStream
    (inReady, outStream) = unbundle outSig

i272o24l6x8Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 24
i272o24l6x8Core (coeffReady, seedStream) =
  (inReady, outStream)
  where
    phase = register Absorb phase'
    keccakState = register 0 keccakState'
    buffer = register Buffer0 buffer'

    (phase', buffer', outSig) =
      unbundle $
        stepControlX8
          <$> phase
          <*> keccakState
          <*> buffer
          <*> coeffReady
          <*> seedStream

    keccakState' = stepStateX8 <$> phase <*> keccakState <*> seedStream
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

{-# ANN
  i272o24l6x2
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
{-# NOINLINE i272o24l6x2 #-}
i272o24l6x2 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l6x2 = toDUT i272o24l6x2Core

{-# ANN
  i272o24l6Merged
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
{-# NOINLINE i272o24l6Merged #-}
i272o24l6Merged ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l6Merged = toDUT i272o24l6MergedCore

{-# ANN
  i272o24l6x3
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
{-# NOINLINE i272o24l6x3 #-}
i272o24l6x3 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l6x3 = toDUT i272o24l6x3Core

{-# ANN
  i272o24l6x4
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
{-# NOINLINE i272o24l6x4 #-}
i272o24l6x4 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l6x4 = toDUT i272o24l6x4Core

{-# ANN
  i272o24l6x6
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
{-# NOINLINE i272o24l6x6 #-}
i272o24l6x6 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l6x6 = toDUT i272o24l6x6Core

{-# ANN
  i272o24l6x8
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
{-# NOINLINE i272o24l6x8 #-}
i272o24l6x8 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
i272o24l6x8 = toDUT i272o24l6x8Core
