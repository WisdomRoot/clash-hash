{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AXI4Stream
  ( AXI4Stream (..),
    AXI4Stream32,
    AXI4Stream64,
    AXI4Stream128,
    Master,
    Slave,
    Pipe,
    PipeCtrl,
    composeSteps,
    mealyCompose,
    (~>),
    toDUT,
    toDUTCtrl,
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

-- | Pipe with an additional control sideband sampled alongside the input stream.
-- Tuple order follows step style: downstream ready, control, upstream stream.
type PipeCtrl dom c a b =
  ( Signal dom Bool,
    Signal dom c,
    Signal dom (AXI4Stream a)
  ) ->
  (Signal dom Bool, Signal dom (AXI4Stream b))

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

mealyCompose ::
  (HiddenClockResetEnable dom, NFDataX s1, NFDataX s2) =>
  (s1 -> (Bool, AXI4Stream a) -> (s1, (Bool, AXI4Stream b))) ->
  s1 ->
  (s2 -> (Bool, AXI4Stream b) -> (s2, (Bool, AXI4Stream c))) ->
  s2 ->
  Pipe dom a c
mealyCompose stepAB initAB stepBC initBC (outReady, inStream) =
  mealyB (composeSteps stepAB stepBC) (initAB, initBC) (outReady, inStream)
{-# INLINE mealyCompose #-}

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
