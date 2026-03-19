module Component.XOF6
  ( i272o72,
    i272o72Core,
  )
where

import AXI4Stream
import Clash.Prelude hiding (tlast)
import Component.XOF qualified as XOF

type InputBits = 272
type OutputBits = 72

data RepackState
  = Empty
  | Have48 (BitVector 48)
  | Have24 (BitVector 24)
  deriving (Show, Eq, Generic, NFDataX)

stepRepack ::
  RepackState ->
  (Bool, AXI4Stream 48) ->
  (RepackState, (Bool, AXI4Stream OutputBits))
stepRepack repackState (outReady, inStream) =
  case repackState of
    Empty ->
      if tvalid inStream
        then (Have48 (tdata inStream), (True, idleAXI4Stream))
        else (Empty, (True, idleAXI4Stream))
    Have48 prev48 ->
      if tvalid inStream
        then
          let curr48 = tdata inStream
              low24 = slice d23 d0 curr48
              high24 = slice d47 d24 curr48
              outBeat = validBeat (low24 ++# prev48) False
           in if outReady
                then (Have24 high24, (True, outBeat))
                else (Have48 prev48, (False, outBeat))
        else (Have48 prev48, (True, idleAXI4Stream))
    Have24 prev24 ->
      if tvalid inStream
        then
          let curr48 = tdata inStream
              outBeat = validBeat (curr48 ++# prev24) False
           in if outReady
                then (Empty, (True, outBeat))
                else (Have24 prev24, (False, outBeat))
        else (Have24 prev24, (True, idleAXI4Stream))

repack72 ::
  HiddenClockResetEnable dom =>
  Pipe dom 48 OutputBits
repack72 (outReady, inStream) =
  mealyB stepRepack Empty (outReady, inStream)

i272o72Core ::
  HiddenClockResetEnable dom =>
  Pipe dom InputBits OutputBits
i272o72Core = XOF.i272o48Core ~> repack72

{-# ANN
  i272o72
  ( Synthesize
      { t_name = "dut",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortProduct
              ""
              [ PortProduct "MSG" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
                PortName "XOF_TREADY"
              ]
          ],
        t_output =
          PortProduct
            ""
            [ PortProduct "XOF" [PortName "TDATA", PortName "TVALID", PortName "TLAST"],
              PortName "MSG_TREADY"
            ]
      }
  )
  #-}
{-# NOINLINE i272o72 #-}
i272o72 ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (AXI4Stream InputBits, Bool) ->
  Signal System (AXI4Stream OutputBits, Bool)
i272o72 = toDUT i272o72Core
