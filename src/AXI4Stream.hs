{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AXI4Stream
  ( AXI4Stream (..),
    AXI4Stream32,
    AXI4Stream64,
    AXI4Stream128,
    Master,
    Slave,
    Pipe,
    Pipe2 (..),
    PipeCtrl,
    PipeCtrl2 (..),
    runPipe2,
    runPipeCtrl2,
    (~>),
    (~>>),
    toDUT,
    toDUT2,
    toDUTCtrl,
    toDUTCtrl2,
    idleAXI4Stream,
    validBeat,
    handshake,
  )
where

import Clash.Prelude hiding (tlast)

--------------------------------------------------------------------------------
-- AXI4-Stream Interface
--------------------------------------------------------------------------------

-- | AXI4-Stream payload/sideband (master-driven signals).
-- The sink's `tready` is modeled separately as a Bool.
data AXI4Stream (n :: Nat) = AXI4Stream
  { tdata :: BitVector n, -- ^ Data payload
    tvalid :: Bool, -- ^ Valid signal
    tlast :: Bool -- ^ Last beat indicator
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

instance Bundle (AXI4Stream n)

--------------------------------------------------------------------------------
-- Common Bus Widths
--------------------------------------------------------------------------------

type AXI4Stream32 = AXI4Stream 32
type AXI4Stream64 = AXI4Stream 64
type AXI4Stream128 = AXI4Stream 128

--------------------------------------------------------------------------------
-- AXI4-Stream Interface Roles
--
-- A pipeline is assembled as:
--
--   @stageA '~>' stageB '~>' stageC@
--
-- where the three roles are:
--
-- * __Source__      (@'Master' dom n@):
--     Produces a stream.  Given @tready@ from downstream, drives
--     @AXI4Stream n@ forward.
--
-- * __Transducer__  (@'AXI4Transducer' dom a b@):
--     Consumes one stream and produces another.
--     It is a slave whose extra output is itself a master.
--
-- * __Sink__        (@'Slave' dom a b@):
--     Consumes a stream and yields a result @b@.
--     Given @AXI4Stream a@, drives @tready@ back and produces @b@.
--------------------------------------------------------------------------------

-- | __Source__ role.
-- Given @tready@ from the slave, drives @AXI4Stream n@ forward.
--
-- Example:
--
-- > counter :: Master dom 32
-- > counter tready = mealy step 0 tready
-- >   where step n _ = (n+1, validBeat n False)
type Master dom n = Signal dom Bool -> Signal dom (AXI4Stream n)

-- | __Sink__ role.
-- Given @AXI4Stream a@ from the master, drives @tready@ back and produces extra output @b@.
--
-- Example:
--
-- > collector :: Slave dom 32 (Signal dom [BitVector 32])
-- > collector stream = (pure True, fmap tdata <$> stream)
type Slave dom a b = Signal dom (AXI4Stream a) -> (Signal dom Bool, b)

-- | __Pipe__ role.
-- A one-step stream component with upstream/downstream ready visibility.
-- Input tuple order: downstream ready, upstream stream.
type Pipe dom a b =
  (Signal dom Bool, Signal dom (AXI4Stream a)) ->
  (Signal dom Bool, Signal dom (AXI4Stream b))

-- | Replacement for 'Pipe'.
--
-- The old 'Pipe' representation is a raw signal function. It is concise, but
-- its composition operator ties the intermediate @tready@ path with a
-- recursive signal knot:
--
-- @
--   stageAB (midReady, inStream)
--   stageBC (outReady, midStream)
-- @
--
-- In hardware that is fine, but in the Haskell/Clash test harness this can
-- become pathologically slow once a downstream stage both:
--
-- * inspects the current input beat, and
-- * deasserts upstream ready while draining buffered output.
--
-- We observed exactly that with @XOF6 -> lookahead4 -> take128@ for
-- @SN-O24-L4@: the DUT was functionally correct, but generic signal-level
-- simulation through the old 'Pipe' composition timed out, while direct
-- step-level simulation of the same logic completed quickly.
--
-- 'Pipe2' avoids that testing problem by allowing a step/state representation,
-- so composition can be performed explicitly at the transition-function level
-- instead of by a recursive signal knot.
--
-- Important: step/state is not the only constructor. Some components rely on
-- preserved signal-level boundaries for good synthesis results. Forcing every
-- component into step/state form can change module structure and increase
-- area. Therefore 'Pipe2' also supports direct signal-level implementations
-- ('Pipe2Net' / 'Pipe2Fn') so we can keep old area behavior where needed while
-- still standardizing on one public abstraction.
data Pipe2 dom a b = forall s.
  NFDataX s =>
  Pipe2
    (s -> (Bool, AXI4Stream a) -> (s, (Bool, AXI4Stream b)))
    s
  | Pipe2Net
      ( HiddenClockResetEnable dom =>
        Signal dom Bool ->
        Signal dom (AXI4Stream a) ->
        (Signal dom Bool, Signal dom (AXI4Stream b))
      )
  | Pipe2Fn (HiddenClockResetEnable dom => Pipe dom a b)

-- | Pipe with an additional control sideband sampled alongside the input stream.
-- Tuple order follows step style: downstream ready, control, upstream stream.
type PipeCtrl dom c a b =
  ( Signal dom Bool,
    Signal dom c,
    Signal dom (AXI4Stream a)
  ) ->
  (Signal dom Bool, Signal dom (AXI4Stream b))

-- | Replacement for 'PipeCtrl'.
--
-- This exists for the same reason as 'Pipe2': the old control-carrying
-- signal-function form is easy to write, but its generic signal-level
-- composition/testing path inherits the same recursive-ready simulation
-- problem as 'Pipe'. For control-carrying components we also need a way to
-- keep proven signal-level boundaries when rewriting to the new abstraction,
-- because some wrappers synthesize much better when that boundary is preserved.
--
-- So, just like 'Pipe2', this type supports both:
--
-- * step/state implementations for robust testing and composition, and
-- * direct signal-level implementations ('PipeCtrl2Net' / 'PipeCtrl2Fn') for
--   area-preserving wrappers.
data PipeCtrl2 dom c a b = forall s.
  NFDataX s =>
  PipeCtrl2
    (s -> (Bool, c, AXI4Stream a) -> (s, (Bool, AXI4Stream b)))
    s
  | PipeCtrl2Net
      ( HiddenClockResetEnable dom =>
        Signal dom Bool ->
        Signal dom c ->
        Signal dom (AXI4Stream a) ->
        (Signal dom Bool, Signal dom (AXI4Stream b))
      )
  | PipeCtrl2Fn (HiddenClockResetEnable dom => PipeCtrl dom c a b)

runPipe2 ::
  HiddenClockResetEnable dom =>
  Pipe2 dom a b ->
  Pipe dom a b
runPipe2 (Pipe2 step initState) = mealyB step initState
runPipe2 (Pipe2Net pipeNet) = \(outReady, inStream) -> pipeNet outReady inStream
runPipe2 (Pipe2Fn pipeFn) = pipeFn
{-# INLINE runPipe2 #-}

runPipeCtrl2 ::
  HiddenClockResetEnable dom =>
  PipeCtrl2 dom c a b ->
  PipeCtrl dom c a b
runPipeCtrl2 (PipeCtrl2 step initState) (outReady, ctrlSig, inStream) =
  mealyB step initState (outReady, ctrlSig, inStream)
runPipeCtrl2 (PipeCtrl2Net pipeNet) (outReady, ctrlSig, inStream) = pipeNet outReady ctrlSig inStream
runPipeCtrl2 (PipeCtrl2Fn pipeFn) args = pipeFn args
{-# INLINE runPipeCtrl2 #-}

composeSteps ::
  (s1 -> (Bool, AXI4Stream a) -> (s1, (Bool, AXI4Stream b))) ->
  (s2 -> (Bool, AXI4Stream b) -> (s2, (Bool, AXI4Stream c))) ->
  (s1, s2) ->
  (Bool, AXI4Stream a) ->
  ((s1, s2), (Bool, AXI4Stream c))
composeSteps stepAB stepBC (stateAB, stateBC) (outReady, inStream) =
  let (stateABFalse, (inReadyFalse, midStreamFalse)) = stepAB stateAB (False, inStream)
      (stateBCFalse, (midReadyFalse, outStreamFalse)) = stepBC stateBC (outReady, midStreamFalse)
   in if not midReadyFalse
        then ((stateABFalse, stateBCFalse), (inReadyFalse, outStreamFalse))
        else
          let (stateABTrue, (inReadyTrue, midStreamTrue)) = stepAB stateAB (True, inStream)
              (stateBCTrue, (midReadyTrue, outStreamTrue)) = stepBC stateBC (outReady, midStreamTrue)
           in if midReadyTrue
                then ((stateABTrue, stateBCTrue), (inReadyTrue, outStreamTrue))
                else error "AXI4Stream.composeSteps: no ready fixed point"
{-# INLINE composeSteps #-}

(~>>) :: Pipe2 dom a b -> Pipe2 dom b c -> Pipe2 dom a c
Pipe2 stepAB initAB ~>> Pipe2 stepBC initBC =
  Pipe2 (composeSteps stepAB stepBC) (initAB, initBC)
lhs ~>> rhs = Pipe2Fn (runPipe2 lhs ~> runPipe2 rhs)
{-# INLINE (~>>) #-}

infixl 1 ~>>

-- | Compose two stream stages, tying the intermediate @tready@ feedback loop.
--
-- The recursive @let@ is safe under Clash's lazy 'Signal' semantics provided
-- there is at least one register in the tready/stream loop (as every real
-- state-machine component has).
--
-- Chains left-associatively: @stageA '~>' stageB '~>' stageC@.
(~>) :: Pipe dom a b -> Pipe dom b c -> Pipe dom a c
stageAB ~> stageBC = \(outReady, inStream) ->
  let (inReady, midStream) = stageAB (midReady, inStream)
      (midReady, outStream) = stageBC (outReady, midStream)
   in (inReady, outStream)
{-# INLINE (~>) #-}

infixl 1 ~>

-- | Adapt a component expressed as @Pipe dom a b@ into the
-- first-order top-entity shape used in this codebase:
--
-- @Signal dom (AXI4Stream a, Bool) -> Signal dom (AXI4Stream b, Bool)@
--
-- where the input Bool is @tready@ for the output stream and the output Bool
-- is @tready@ for the input stream.
toDUT ::
  KnownDomain dom =>
  (HiddenClockResetEnable dom => Pipe dom a b) ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom (AXI4Stream a, Bool) ->
  Signal dom (AXI4Stream b, Bool)
toDUT comp clk rst en inputSig =
  withClockResetEnable clk rst en $
    let (inputStream, outReady) = unbundle inputSig
        (inReady, outStream) = comp (outReady, inputStream)
     in bundle (outStream, inReady)

toDUT2 ::
  KnownDomain dom =>
  Pipe2 dom a b ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom (AXI4Stream a, Bool) ->
  Signal dom (AXI4Stream b, Bool)
toDUT2 (Pipe2Fn pipeFn) clk rst en inputSig = toDUT pipeFn clk rst en inputSig
toDUT2 (Pipe2Net pipeNet) clk rst en inputSig =
  withClockResetEnable clk rst en $
    let (inputStream, outReady) = unbundle inputSig
        (inReady, outStream) = pipeNet outReady inputStream
     in bundle (outStream, inReady)
toDUT2 comp clk rst en inputSig =
  withClockResetEnable clk rst en $
    let (inputStream, outReady) = unbundle inputSig
        (inReady, outStream) = runPipe2 comp (outReady, inputStream)
     in bundle (outStream, inReady)

-- | Adapt a component expressed as @PipeCtrl dom c a b@ into a first-order
-- top-entity shape with explicit downstream @tready@ and input-side control.
toDUTCtrl ::
  KnownDomain dom =>
  (HiddenClockResetEnable dom => PipeCtrl dom c a b) ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom Bool ->
  Signal dom (AXI4Stream a, c) ->
  Signal dom (AXI4Stream b, Bool)
toDUTCtrl comp clk rst en outReady inputSig =
  withClockResetEnable clk rst en $
    let (inputStream, ctrlSig) = unbundle inputSig
        (inReady, outStream) = comp (outReady, ctrlSig, inputStream)
     in bundle (outStream, inReady)

toDUTCtrl2 ::
  KnownDomain dom =>
  PipeCtrl2 dom c a b ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Signal dom Bool ->
  Signal dom (AXI4Stream a, c) ->
  Signal dom (AXI4Stream b, Bool)
toDUTCtrl2 (PipeCtrl2Fn pipeFn) clk rst en outReady inputSig = toDUTCtrl pipeFn clk rst en outReady inputSig
toDUTCtrl2 (PipeCtrl2Net pipeNet) clk rst en outReady inputSig =
  withClockResetEnable clk rst en $
    let (inputStream, ctrlSig) = unbundle inputSig
        (inReady, outStream) = pipeNet outReady ctrlSig inputStream
     in bundle (outStream, inReady)
toDUTCtrl2 comp clk rst en outReady inputSig =
  withClockResetEnable clk rst en $
    let (inputStream, ctrlSig) = unbundle inputSig
        (inReady, outStream) = runPipeCtrl2 comp (outReady, ctrlSig, inputStream)
     in bundle (outStream, inReady)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

idleAXI4Stream :: (KnownNat n) => AXI4Stream n
idleAXI4Stream =
  AXI4Stream
    { tdata = 0,
      tvalid = False,
      tlast = False
    }

validBeat :: (KnownNat n) => BitVector n -> Bool -> AXI4Stream n
validBeat dat isLast =
  AXI4Stream
    { tdata = dat,
      tvalid = True,
      tlast = isLast
    }

handshake :: Bool -> AXI4Stream n -> Bool
handshake tready axi = tvalid axi && tready
