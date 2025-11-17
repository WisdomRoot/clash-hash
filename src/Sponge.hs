{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Sponge
  ( -- * AXI4-Stream sponge
    spongeAxi,
  )
where

import Clash.Prelude

--------------------------------------------------------------------------------
-- AXI4-Stream Sponge
--------------------------------------------------------------------------------

-- | Control FSM, split into 4 explicit modes
data Ctrl rate rounds digestBlocks
  = AbsIdle
      { padPending :: Bool,
        padBlock :: BitVector rate
      }
  | AbsBusy
      { roundCnt :: Index rounds,
        seenTLast :: Bool,
        padPending :: Bool,
        padBlock :: BitVector rate
      }
  | SqIdle
      { currentBlock :: BitVector rate,
        digestPending :: Bool,
        squeezeRem :: Index digestBlocks
      }
  | SqBusy
      { roundCnt :: Index rounds,
        digestPending :: Bool,
        squeezeRem :: Index digestBlocks
      }
  deriving stock (Generic)
  deriving anyclass (NFDataX)

-- | Mealy input bundle (clarity)
data In rate = In
  { sValid :: Bool,
    sData :: BitVector rate,
    sLast :: Bool,
    mReady :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (NFDataX)

-- | Mealy output bundle (clarity)
data Out rate = Out
  { sReady :: Bool,
    mValid :: Bool,
    mData :: BitVector rate,
    mLast :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (NFDataX)

-- | Generic AXI4-Stream sponge construction.
--
-- = Parameters
--
-- * @b@ - Permutation width (state size)
-- * @rate@ - Rate (bits absorbed/squeezed per permutation)
-- * @digest@ - Output digest size in bits
-- * @rounds@ - Number of permutation rounds
--
-- = AXI4-Stream Interface
--
-- * Input (slave): s_axis_tvalid, s_axis_tdata (rate bits), s_axis_tlast, s_axis_tready (output)
-- * Output (master): m_axis_tvalid, m_axis_tdata (rate bits), m_axis_tlast, m_axis_tready
--
-- = Operation
--
-- Accepts raw input blocks on AXI stream. TLAST marks final full-width data block.
-- Automatically constructs and injects pad10*1 block after TLAST.
-- Outputs digest in rate-bit blocks. Supports multi-cycle squeezing when digest > rate.
--
-- = Domain Separation Suffix
--
-- The 2-bit suffix parameter specifies the domain separation:
-- * @0b01@ - SHA3 hash functions
-- * @0b11@ - SHAKE extendable-output functions
-- * @0b10@ - RawSHAKE (raw Keccak)
--
-- The padding block format is: suffix || 1 || 0...0 || 1 (pad10*1 rule)
spongeAxi ::
  forall dom b rate digest rounds digestBlocks.
  ( HiddenClockResetEnable dom,
    KnownNat b,
    KnownNat rate,
    KnownNat digest,
    KnownNat rounds,
    KnownNat digestBlocks,
    digestBlocks ~ DivRU digest rate,
    1 <= b,
    4 <= rate,
    1 <= digest,
    1 <= rounds,
    1 <= digestBlocks,
    rate <= b,
    digest <= b
  ) =>
  BitVector 2 -> -- Domain separation suffix (0b01=SHA3, 0b11=SHAKE, 0b10=RawSHAKE)
  (Signal dom (Index rounds, BitVector b) -> Signal dom (BitVector b)) -> -- Permutation component
  Signal dom Bool -> -- s_axis_tvalid
  Signal dom (BitVector rate) -> -- s_axis_tdata
  Signal dom Bool -> -- s_axis_tlast
  Signal dom Bool -> -- m_axis_tready
  ( Signal dom Bool, -- s_axis_tready
    Signal dom Bool, -- m_axis_tvalid
    Signal dom (BitVector rate), -- m_axis_tdata
    Signal dom Bool -- m_axis_tlast
  )
spongeAxi suffix permutationComponent sAxisTValid sAxisTData sAxisTLast mAxisTReady =
  (sReady <$> axiOutputs, mValid <$> axiOutputs, mData <$> axiOutputs, mLast <$> axiOutputs)
  where
    -- Bundle AXI inputs
    axiInputs = In <$> sAxisTValid <*> sAxisTData <*> sAxisTLast <*> mAxisTReady

    -- FSM with feedback loop through permutation component
    (axiOutputs, permIn) = unbundle $ mealy step initialState (bundle (axiInputs, stateDataAfterPerm))

    -- Permutation component instantiation
    stateDataAfterPerm = permutationComponent permIn

    initialCtrl =
      AbsIdle
        { padPending = False,
          padBlock = 0
        }

    initialState = (0 :: BitVector b, initialCtrl)

    maxRound = maxBound :: Index rounds

    totalDigestBlocks = natToNum @digestBlocks :: Index digestBlocks

    padBlockVal :: BitVector rate
    padBlockVal = suffixPadBlock suffix

    step ::
      (BitVector b, Ctrl rate rounds digestBlocks) ->
      (In rate, BitVector b) ->
      ((BitVector b, Ctrl rate rounds digestBlocks), (Out rate, (Index rounds, BitVector b)))
    step (stBV, ctrl) (inp, permOut) =
      let haveInput = sValid inp
          lastBeat = sLast inp
          canOutput = case ctrl of
            SqIdle {digestPending = True} -> mReady inp
            _ -> False

          -- Default values
          round0 = 0 :: Index rounds
          -- Split permOut once to get rate portion directly (avoid resize on 200-bit net)
          (_capFromPerm, rateFromPerm) = split permOut :: (BitVector (b - rate), BitVector rate)
          blockFromSt = rateFromPerm

          (stAfterAbsorb, roundCntNext, ctrlNext) =
            case ctrl of
              AbsIdle {..}
                | haveInput ->
                    let (cap, rateBits) = split stBV :: (BitVector (b - rate), BitVector rate)
                        rate' = rateBits `xor` sData inp :: BitVector rate
                        st' = cap ++# rate' :: BitVector b
                        padP = lastBeat
                        padB = if lastBeat then padBlockVal else padBlock
                     in ( st',
                          round0,
                          AbsBusy
                            { roundCnt = round0,
                              seenTLast = padP,
                              padPending = padP,
                              padBlock = padB
                            }
                        )
                | padPending ->
                    let (cap, rateBits) = split stBV :: (BitVector (b - rate), BitVector rate)
                        rate' = rateBits `xor` padBlock :: BitVector rate
                        st' = cap ++# rate' :: BitVector b
                     in ( st',
                          round0,
                          AbsBusy
                            { roundCnt = round0,
                              seenTLast = True,
                              padPending = False,
                              padBlock = 0
                            }
                        )
                | otherwise -> (stBV, round0, ctrl)
              AbsBusy {..}
                | roundCnt == maxRound ->
                    if seenTLast
                      then
                        ( permOut,  -- CRITICAL: Write back permutation result
                          round0,
                          SqIdle
                            { currentBlock = blockFromSt,
                              digestPending = True,
                              squeezeRem = totalDigestBlocks - 1
                            }
                        )
                      else
                        ( permOut,  -- CRITICAL: Write back permutation result
                          round0,
                          AbsIdle
                            { padPending = padPending,
                              padBlock = padBlock
                            }
                        )
                | otherwise ->
                    ( stBV,
                      roundCnt + 1,
                      AbsBusy
                        { roundCnt = roundCnt + 1,
                          seenTLast = seenTLast,
                          padPending = padPending,
                          padBlock = padBlock
                        }
                    )
              SqIdle {..}
                | canOutput && squeezeRem == 0 ->
                    ( stBV,
                      round0,
                      AbsIdle
                        { padPending = False,
                          padBlock = 0
                        }
                    )
                | canOutput ->
                    ( stBV,
                      round0,
                      SqBusy
                        { roundCnt = round0,
                          digestPending = False,
                          squeezeRem = squeezeRem - 1
                        }
                    )
                | otherwise -> (stBV, round0, ctrl)
              SqBusy {..}
                | roundCnt == maxRound ->
                    ( permOut,  -- CRITICAL: Write back permutation result
                      round0,
                      SqIdle
                        { currentBlock = blockFromSt,
                          digestPending = True,
                          squeezeRem = squeezeRem
                        }
                    )
                | otherwise ->
                    ( stBV,
                      roundCnt + 1,
                      SqBusy
                        { roundCnt = roundCnt + 1,
                          digestPending = digestPending,
                          squeezeRem = squeezeRem
                        }
                    )

          -- Outputs from CURRENT control state (not ctrlNext)
          -- CRITICAL: AXI handshake must reflect current cycle's state
          out = case ctrl of
            AbsIdle {} -> Out {sReady = True, mValid = False, mData = 0, mLast = False}
            AbsBusy {} -> Out {sReady = False, mValid = False, mData = 0, mLast = False}
            SqIdle {..} -> Out {sReady = False, mValid = digestPending, mData = currentBlock, mLast = squeezeRem == 0}
            SqBusy {} -> Out {sReady = False, mValid = False, mData = 0, mLast = False}

          -- Permutation input uses updated state after absorption
          permutationInput = (roundCntNext, stAfterAbsorb)
       in ((stAfterAbsorb, ctrlNext), (out, permutationInput))

    suffixPadBlock :: BitVector 2 -> BitVector rate
    suffixPadBlock suff =
      let suffixBits :: BitVector rate
          suffixBits = setBitIf (testBit suff 0) 1 $ setBitIf (testBit suff 1) 0 (0 :: BitVector rate)
          firstPadBit = setBit (0 :: BitVector rate) 2
          finalPadBit = setBit (0 :: BitVector rate) (rateWidth - 1)
       in suffixBits .|. firstPadBit .|. finalPadBit

    rateWidth :: Int
    rateWidth = natToNum @rate

    setBitIf :: Bool -> Int -> BitVector rate -> BitVector rate
    setBitIf cond idx bv = if cond then setBit bv idx else bv
