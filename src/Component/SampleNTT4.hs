module Component.SampleNTT4
  ( i272o24l4,
    i272o24l4Core,
    lookahead4,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.XOF6 qualified as XOF6

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

stepLookahead4 ::
  Buffer ->
  (Bool, AXI4Stream 72) ->
  (Buffer, (Bool, AXI4Stream 24))
stepLookahead4 buffer (coeffReady, candidateStream) =
  case buffer of
    Buffer0 ->
      if tvalid candidateStream
        then consumeChunk Buffer0 (screenCandidates (tdata candidateStream))
        else (Buffer0, (True, idleAXI4Stream))
    Buffer1 b0 ->
      if tvalid candidateStream
        then consumeChunk (Buffer1 b0) (screenCandidates (tdata candidateStream))
        else (Buffer1 b0, (True, idleAXI4Stream))
    _ -> drainBuffer buffer
  where
    drainBuffer pending =
      let (pair, pending') = popPair pending
          outStream = validBeat pair False
       in if coeffReady
            then (pending', (False, outStream))
            else (pending, (False, outStream))

    consumeChunk current candidates = case current of
      Buffer0 -> case candidates of
        Valid0 -> (Buffer0, (True, idleAXI4Stream))
        Valid1 c0 -> (Buffer1 c0, (True, idleAXI4Stream))
        Valid2 c0 c1 ->
          if coeffReady
            then (Buffer0, (True, validBeat (c1 ++# c0) False))
            else (Buffer2 c0 c1, (True, idleAXI4Stream))
        Valid3 c0 c1 c2 ->
          if coeffReady
            then (Buffer1 c2, (True, validBeat (c1 ++# c0) False))
            else (Buffer3 c0 c1 c2, (True, idleAXI4Stream))
        Valid4 c0 c1 c2 c3 ->
          if coeffReady
            then (Buffer2 c2 c3, (True, validBeat (c1 ++# c0) False))
            else (Buffer4 c0 c1 c2 c3, (True, idleAXI4Stream))
        Valid5 c0 c1 c2 c3 c4 ->
          if coeffReady
            then (Buffer3 c2 c3 c4, (True, validBeat (c1 ++# c0) False))
            else (Buffer5 c0 c1 c2 c3 c4, (True, idleAXI4Stream))
        Valid6 c0 c1 c2 c3 c4 c5 ->
          if coeffReady
            then (Buffer4 c2 c3 c4 c5, (True, validBeat (c1 ++# c0) False))
            else (Buffer6 c0 c1 c2 c3 c4 c5, (True, idleAXI4Stream))
      Buffer1 b0 -> case candidates of
        Valid0 -> (Buffer1 b0, (True, idleAXI4Stream))
        Valid1 c0 ->
          if coeffReady
            then (Buffer0, (True, validBeat (c0 ++# b0) False))
            else (Buffer2 b0 c0, (True, idleAXI4Stream))
        Valid2 c0 c1 ->
          if coeffReady
            then (Buffer1 c1, (True, validBeat (c0 ++# b0) False))
            else (Buffer3 b0 c0 c1, (True, idleAXI4Stream))
        Valid3 c0 c1 c2 ->
          if coeffReady
            then (Buffer2 c1 c2, (True, validBeat (c0 ++# b0) False))
            else (Buffer4 b0 c0 c1 c2, (True, idleAXI4Stream))
        Valid4 c0 c1 c2 c3 ->
          if coeffReady
            then (Buffer3 c1 c2 c3, (True, validBeat (c0 ++# b0) False))
            else (Buffer5 b0 c0 c1 c2 c3, (True, idleAXI4Stream))
        Valid5 c0 c1 c2 c3 c4 ->
          if coeffReady
            then (Buffer4 c1 c2 c3 c4, (True, validBeat (c0 ++# b0) False))
            else (Buffer6 b0 c0 c1 c2 c3 c4, (True, idleAXI4Stream))
        Valid6 c0 c1 c2 c3 c4 c5 ->
          if coeffReady
            then (Buffer5 c1 c2 c3 c4 c5, (True, validBeat (c0 ++# b0) False))
            else (Buffer7 b0 c0 c1 c2 c3 c4 c5, (True, idleAXI4Stream))
      _ -> error "Component.SampleNTT4.consumeChunk: invalid buffer state"

lookahead4 ::
  HiddenClockResetEnable dom =>
  Pipe dom 72 24
lookahead4 (coeffReady, candidateStream) =
  mealyB stepLookahead4 Buffer0 (coeffReady, candidateStream)

i272o24l4Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 24
i272o24l4Core = XOF6.i272o72Core ~> lookahead4

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
