module Component.SampleNTT512B
  ( buffer,
    i272o24b60,
    topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.XOF qualified as XOF

data Buffer
  = Buffer0
  | Buffer1 (BitVector 12)
  | Buffer2 (BitVector 12) (BitVector 12)
  | Buffer3 (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer4 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer5 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  deriving (Show, Eq, Generic, NFDataX)

data Candidates
  = Valid0
  | Valid1 (BitVector 12)
  | Valid2 (BitVector 12) (BitVector 12)
  | Valid3 (BitVector 12) (BitVector 12) (BitVector 12)
  | Valid4 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
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

bufferStep ::
  Buffer ->
  (Bool, AXI4Stream 48) ->
  (Buffer, (Bool, AXI4Stream 24))
bufferStep buf (outReady, AXI4Stream chunk inValid _) =
  case buf of
    Buffer0 ->
      if not inValid
        then (Buffer0, (True, idleAXI4Stream))
        else
          case screenCandidates chunk of
            Valid0 -> (Buffer0, (True, idleAXI4Stream))
            Valid1 c0 -> (Buffer1 c0, (True, idleAXI4Stream))
            Valid2 c0 c1 ->
              if outReady
                then (Buffer0, (True, validBeat (c1 ++# c0) False))
                else (Buffer2 c0 c1, (True, idleAXI4Stream))
            Valid3 c0 c1 c2 ->
              if outReady
                then (Buffer1 c2, (True, validBeat (c1 ++# c0) False))
                else (Buffer3 c0 c1 c2, (True, idleAXI4Stream))
            Valid4 c0 c1 c2 c3 ->
              if outReady
                then (Buffer2 c2 c3, (True, validBeat (c1 ++# c0) False))
                else (Buffer4 c0 c1 c2 c3, (True, idleAXI4Stream))
    Buffer1 b0 ->
      if not inValid
        then (Buffer1 b0, (True, idleAXI4Stream))
        else
          case screenCandidates chunk of
            Valid0 -> (Buffer1 b0, (True, idleAXI4Stream))
            Valid1 c0 ->
              if outReady
                then (Buffer0, (True, validBeat (c0 ++# b0) False))
                else (Buffer2 b0 c0, (True, idleAXI4Stream))
            Valid2 c0 c1 ->
              if outReady
                then (Buffer1 c1, (True, validBeat (c0 ++# b0) False))
                else (Buffer3 b0 c0 c1, (True, idleAXI4Stream))
            Valid3 c0 c1 c2 ->
              if outReady
                then (Buffer2 c1 c2, (True, validBeat (c0 ++# b0) False))
                else (Buffer4 b0 c0 c1 c2, (True, idleAXI4Stream))
            Valid4 c0 c1 c2 c3 ->
              if outReady
                then (Buffer3 c1 c2 c3, (True, validBeat (c0 ++# b0) False))
                else (Buffer5 b0 c0 c1 c2 c3, (True, idleAXI4Stream))
    Buffer2 b0 b1 ->
      if inValid
        then (Buffer0, (False, validBeat (b1 ++# b0) False))
        else (Buffer2 b0 b1, (True, idleAXI4Stream))
    Buffer3 b0 b1 b2 ->
      if inValid
        then (Buffer1 b2, (False, validBeat (b1 ++# b0) False))
        else (Buffer3 b0 b1 b2, (True, idleAXI4Stream))
    Buffer4 b0 b1 b2 b3 ->
      if inValid
        then (Buffer2 b2 b3, (False, validBeat (b1 ++# b0) False))
        else (Buffer4 b0 b1 b2 b3, (True, idleAXI4Stream))
    Buffer5 b0 b1 b2 b3 b4 ->
      if inValid
        then (Buffer3 b2 b3 b4, (False, validBeat (b1 ++# b0) False))
        else (Buffer5 b0 b1 b2 b3 b4, (True, idleAXI4Stream))

buffer ::
  HiddenClockResetEnable dom =>
  Stage dom 48 24
buffer (coeffReady, coeff48Stream) =
  mealyB bufferStep Buffer0 (coeffReady, coeff48Stream)

xof ::
  HiddenClockResetEnable dom =>
  Stage dom 272 48
xof (xofReady, seedStream) =
  let widenSeed s =
        AXI4Stream
          { tdata = (0 :: BitVector 2) ++# tdata s,
            tvalid = tvalid s,
            tlast = tlast s
          }
   in XOF.i272o48 (xofReady, fmap widenSeed seedStream)

i272o24b60 ::
  HiddenClockResetEnable dom =>
  Stage dom 272 24
i272o24b60 = xof ~> buffer

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "SN512_I272_O24_B60",
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
{-# NOINLINE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream 272, Bool) ->
  Signal System (AXI4Stream 24, Bool)
topEntity = stageTop i272o24b60
