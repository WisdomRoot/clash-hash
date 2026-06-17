{-# LANGUAGE TemplateHaskell #-}

module Component.SampleNTT6O48
  ( i272o48l6,
    i272o48l6Core,
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
  | Buffer10 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer11 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  deriving (Show, Eq, Generic, NFDataX)

data Phase = Absorb | Permute (Index 24) | Squeeze (Index 14)
  deriving (Show, Eq, Generic, NFDataX)

data State
  = State Phase (BitVector 1600) Buffer
  deriving (Show, Eq, Generic, NFDataX)

$(mkRead "squeezeCoeff96" 1600 [(i, i * 96, 96) | i <- [0 .. 13]])

{-# INLINE squeezeCoeff96 #-}

pushBuffer :: Buffer -> BitVector 12 -> Buffer
pushBuffer Buffer0 c0 = Buffer1 c0
pushBuffer (Buffer1 c0) c1 = Buffer2 c0 c1
pushBuffer (Buffer2 c0 c1) c2 = Buffer3 c0 c1 c2
pushBuffer (Buffer3 c0 c1 c2) c3 = Buffer4 c0 c1 c2 c3
pushBuffer (Buffer4 c0 c1 c2 c3) c4 = Buffer5 c0 c1 c2 c3 c4
pushBuffer (Buffer5 c0 c1 c2 c3 c4) c5 = Buffer6 c0 c1 c2 c3 c4 c5
pushBuffer (Buffer6 c0 c1 c2 c3 c4 c5) c6 = Buffer7 c0 c1 c2 c3 c4 c5 c6
pushBuffer (Buffer7 c0 c1 c2 c3 c4 c5 c6) c7 = Buffer8 c0 c1 c2 c3 c4 c5 c6 c7
pushBuffer (Buffer8 c0 c1 c2 c3 c4 c5 c6 c7) c8 = Buffer9 c0 c1 c2 c3 c4 c5 c6 c7 c8
pushBuffer (Buffer9 c0 c1 c2 c3 c4 c5 c6 c7 c8) c9 = Buffer10 c0 c1 c2 c3 c4 c5 c6 c7 c8 c9
pushBuffer (Buffer10 c0 c1 c2 c3 c4 c5 c6 c7 c8 c9) c10 = Buffer11 c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
pushBuffer buffer _ = buffer

screenIntoBuffer :: Buffer -> BitVector 96 -> Buffer
screenIntoBuffer buffer chunk =
  foldl
    (\pending (isValid, candidate) -> if isValid then pushBuffer pending candidate else pending)
    buffer
    (screenCoeff96 chunk)
{-# INLINE screenIntoBuffer #-}

popQuad :: Buffer -> (BitVector 48, Buffer)
popQuad (Buffer4 a b c d) = (d ++# c ++# b ++# a, Buffer0)
popQuad (Buffer5 a b c d e) = (d ++# c ++# b ++# a, Buffer1 e)
popQuad (Buffer6 a b c d e f) = (d ++# c ++# b ++# a, Buffer2 e f)
popQuad (Buffer7 a b c d e f g) = (d ++# c ++# b ++# a, Buffer3 e f g)
popQuad (Buffer8 a b c d e f g h) = (d ++# c ++# b ++# a, Buffer4 e f g h)
popQuad (Buffer9 a b c d e f g h i) = (d ++# c ++# b ++# a, Buffer5 e f g h i)
popQuad (Buffer10 a b c d e f g h i j) = (d ++# c ++# b ++# a, Buffer6 e f g h i j)
popQuad (Buffer11 a b c d e f g h i j k) = (d ++# c ++# b ++# a, Buffer7 e f g h i j k)
popQuad _ = error "Component.SampleNTT6O48.popQuad: buffer underflow"
{-# INLINE popQuad #-}

canPopQuad :: Buffer -> Bool
canPopQuad Buffer0 = False
canPopQuad (Buffer1 _) = False
canPopQuad (Buffer2 _ _) = False
canPopQuad (Buffer3 _ _ _) = False
canPopQuad _ = True
{-# INLINE canPopQuad #-}

stepControl ::
  Phase ->
  BitVector 1600 ->
  Buffer ->
  Bool ->
  AXI4Stream 272 ->
  (Phase, Buffer, (Bool, AXI4Stream 48))
stepControl phase state buffer tready (AXI4Stream _ msgValid _) = case phase of
  Absorb ->
    if msgValid
      then (Permute 0, Buffer0, (False, idleAXI4Stream))
      else (Absorb, buffer, (True, idleAXI4Stream))
  Permute counter ->
    if counter == maxBound
      then (Squeeze 0, buffer, (False, idleAXI4Stream))
      else (Permute (counter + 1), buffer, (False, idleAXI4Stream))
  Squeeze counter ->
    if canPopQuad buffer
      then
        let (quad, buffer') = popQuad buffer
         in if tready
              then (Squeeze counter, buffer', (False, validBeat quad False))
              else (Squeeze counter, buffer, (False, validBeat quad False))
      else
        let chunk = squeezeCoeff96 state counter
            nextPhase = if counter == maxBound then Permute 0 else Squeeze (counter + 1)
            buffer' = screenIntoBuffer buffer chunk
         in if canPopQuad buffer' && tready
              then
                let (quad, buffer'') = popQuad buffer'
                 in (nextPhase, buffer'', (False, validBeat quad False))
              else (nextPhase, buffer', (False, idleAXI4Stream))

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

i272o48l6Core ::
  HiddenClockResetEnable dom =>
  Pipe dom 272 48
i272o48l6Core (coeffReady, seedStream) =
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
  i272o48l6
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
{-# NOINLINE i272o48l6 #-}
i272o48l6 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 48, Bool)
i272o48l6 = toDUT i272o48l6Core
