module Component.SampleNTT512B
  ( i272o24b60,
    topEntity,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.XOF qualified as XOF
import Prelude qualified as P

data Buffer
  = Buffer0
  | Buffer1 (BitVector 12)
  | Buffer2 (BitVector 12) (BitVector 12)
  | Buffer3 (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer4 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  | Buffer5 (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12) (BitVector 12)
  deriving (Show, Eq, Generic, NFDataX)

validCoeffs :: BitVector 48 -> [BitVector 12]
validCoeffs chunk =
  let c0 = slice (SNat @11) (SNat @0) chunk
      c1 = slice (SNat @23) (SNat @12) chunk
      c2 = slice (SNat @35) (SNat @24) chunk
      c3 = slice (SNat @47) (SNat @36) chunk
      keep c = unpack c < (3329 :: Unsigned 12)
   in [c | c <- [c0, c1, c2, c3], keep c]

appendCoeff :: Buffer -> BitVector 12 -> Buffer
appendCoeff buf c =
  case buf of
    Buffer0 -> Buffer1 c
    Buffer1 b0 -> Buffer2 b0 c
    Buffer2 b0 b1 -> Buffer3 b0 b1 c
    Buffer3 b0 b1 b2 -> Buffer4 b0 b1 b2 c
    Buffer4 b0 b1 b2 b3 -> Buffer5 b0 b1 b2 b3 c
    Buffer5 {} -> error "SampleNTT512B: 60-bit buffer overflow"

appendMany :: Buffer -> [BitVector 12] -> Buffer
appendMany = P.foldl appendCoeff

popPair :: Buffer -> Maybe ((BitVector 12, BitVector 12), Buffer)
popPair buf =
  case buf of
    Buffer2 b0 b1 -> Just ((b0, b1), Buffer0)
    Buffer3 b0 b1 b2 -> Just ((b0, b1), Buffer1 b2)
    Buffer4 b0 b1 b2 b3 -> Just ((b0, b1), Buffer2 b2 b3)
    Buffer5 b0 b1 b2 b3 b4 -> Just ((b0, b1), Buffer3 b2 b3 b4)
    _ -> Nothing

bufferStep ::
  Buffer ->
  (Bool, AXI4Stream 48) ->
  (Buffer, (Bool, AXI4Stream 24))
bufferStep buf (outReady, AXI4Stream chunk inValid _) =
  case popPair buf of
    Just ((c0, c1), nextBuf) ->
      let out = validBeat (c1 ++# c0) False
       in if outReady
            then (nextBuf, (False, out))
            else (buf, (False, out))
    Nothing ->
      if not inValid
        then (buf, (True, idleAXI4Stream))
        else
          let buf' = appendMany buf (validCoeffs chunk)
           in case popPair buf' of
                Just ((c0, c1), nextBuf) ->
                  let out = validBeat (c1 ++# c0) False
                   in if outReady
                        then (nextBuf, (True, out))
                        else (buf', (True, out))
                Nothing -> (buf', (True, idleAXI4Stream))

i272o24b60 ::
  HiddenClockResetEnable dom =>
  Stage dom 272 24
i272o24b60 (coeffReady, seedStream) =
  let widenSeed s =
        AXI4Stream
          { tdata = (0 :: BitVector 2) ++# tdata s,
            tvalid = tvalid s,
            tlast = tlast s
          }
      seedStream274 = fmap widenSeed seedStream
      (xofReady, coeff48Stream) = mealyB bufferStep Buffer0 (coeffReady, xofStream)
      (seedReady, xofStream) = XOF.i272o48 (xofReady, seedStream274)
   in (seedReady, coeff48Stream)

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
