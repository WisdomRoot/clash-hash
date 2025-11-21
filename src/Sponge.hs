{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
data Ctrl bus rate rounds digestBlocks beatsPerBlock
  = AbsIdle
      { padPending :: Bool,
        padBlock :: BitVector rate,
        beatCount :: Index beatsPerBlock
      }
  | AbsBusy
      { roundCnt :: Index rounds,
        seenTLast :: Bool,
        padPending :: Bool,
        padBlock :: BitVector rate,
        beatCount :: Index beatsPerBlock
      }
  | SqIdle
      { currentBlock :: BitVector rate,
        digestPending :: Bool,
        squeezeRem :: Index digestBlocks,
        beatCount :: Index beatsPerBlock
      }
  | SqBusy
      { roundCnt :: Index rounds,
        digestPending :: Bool,
        squeezeRem :: Index digestBlocks,
        beatCount :: Index beatsPerBlock
      }
  deriving stock (Generic)
  deriving anyclass (NFDataX)

-- | Mealy input bundle (clarity)
data In bus = In
  { sValid :: Bool,
    sData :: BitVector bus,
    sLast :: Bool,
    mReady :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (NFDataX)

-- | Mealy output bundle (clarity)
data Out bus = Out
  { sReady :: Bool,
    mValid :: Bool,
    mData :: BitVector bus,
    mLast :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (NFDataX)

-- | Generic AXI4-Stream sponge construction.
--
-- = Parameters
--
-- * @b@ - Permutation width (state size)
-- * @bus@ - AXI4-Stream bus width (typically 64 bits)
-- * @rate@ - Rate (bits absorbed/squeezed per permutation)
-- * @digest@ - Output digest size in bits
-- * @rounds@ - Number of permutation rounds
--
-- = AXI4-Stream Interface
--
-- * Input (slave): s_axis_tvalid, s_axis_tdata (bus bits), s_axis_tlast, s_axis_tready (output)
-- * Output (master): m_axis_tvalid, m_axis_tdata (bus bits), m_axis_tlast, m_axis_tready
--
-- = Operation
--
-- Accepts raw input blocks on AXI stream. TLAST marks final full-width data block.
-- Automatically constructs and injects pad10*1 block after TLAST.
-- Outputs digest in bus-bit blocks. Supports multi-cycle squeezing when digest > bus.
-- When rate > bus, multiple beats are gathered before permutation.
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
  forall dom b bus rate digest rounds digestBlocks beatsPerBlock.
  ( HiddenClockResetEnable dom,
    KnownNat b,
    KnownNat bus,
    KnownNat rate,
    KnownNat digest,
    KnownNat rounds,
    KnownNat digestBlocks,
    KnownNat beatsPerBlock,
    digestBlocks ~ DivRU digest rate,
    beatsPerBlock ~ Div rate bus,
    beatsPerBlock * bus ~ rate,
    1 <= b,
    1 <= bus,
    4 <= rate,
    1 <= digest,
    1 <= rounds,
    1 <= digestBlocks,
    1 <= beatsPerBlock,
    bus <= rate,
    rate <= b,
    rate `Mod` bus ~ 0,
    digest <= b
  ) =>
  BitVector 2 -> -- Domain separation suffix (0b01=SHA3, 0b11=SHAKE, 0b10=RawSHAKE)
  (Signal dom (Index rounds, BitVector b) -> Signal dom (BitVector b)) -> -- Permutation component
  Signal dom Bool -> -- s_axis_tvalid
  Signal dom (BitVector bus) -> -- s_axis_tdata
  Signal dom Bool -> -- s_axis_tlast
  Signal dom Bool -> -- m_axis_tready
  ( Signal dom Bool, -- s_axis_tready
    Signal dom Bool, -- m_axis_tvalid
    Signal dom (BitVector bus), -- m_axis_tdata
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
          padBlock = 0,
          beatCount = 0
        }

    initialState = (0 :: BitVector b, initialCtrl)

    maxRound = maxBound :: Index rounds

    -- Number of remaining digest blocks after the current one.
    -- For Index k the maximum value is k-1, which is exactly the count of
    -- extra blocks we still need to emit.
    totalDigestBlocks = maxBound :: Index digestBlocks

    maxBeatCount = maxBound :: Index beatsPerBlock

    padBlockVal :: BitVector rate
    padBlockVal = suffixPadBlock suffix

    step ::
      (BitVector b, Ctrl bus rate rounds digestBlocks beatsPerBlock) ->
      (In bus, BitVector b) ->
      ((BitVector b, Ctrl bus rate rounds digestBlocks beatsPerBlock), (Out bus, (Index rounds, BitVector b)))
    step (stBV, ctrl) (inp, permOut) =
      let -- AXI handshake: only consume data when both TVALID and TREADY are high
          inputReady = case ctrl of
            AbsIdle padPending_check _ _ -> not padPending_check
            _ -> False
          haveInput = sValid inp && inputReady
          lastBeat = sLast inp
          canOutput = case ctrl of
            SqIdle {digestPending = True} -> mReady inp
            _ -> False

          -- Default values
          round0 = 0 :: Index rounds
          -- Split permOut once to get rate portion directly (avoid resize on 200-bit net)
          (_capFromPerm, rateFromPerm) = split permOut :: (BitVector (b - rate), BitVector rate)
          blockFromSt = rateFromPerm

          -- Helper: write a bus-sized slice into the rate portion at the given beat index
          -- XORs sliceData with the current chunk at beatIdx
          writeSlice :: BitVector rate -> Index beatsPerBlock -> BitVector bus -> BitVector rate
          writeSlice rateBits beatIdx sliceData =
            let chunks :: Vec beatsPerBlock (BitVector bus)
                chunks = bitCoerce rateBits
                oldChunk = chunks !! beatIdx
                newChunk = oldChunk `xor` sliceData
                chunks' = replace beatIdx newChunk chunks
             in bitCoerce chunks'

          -- Helper: read a bus-sized slice from the rate portion at the given beat index
          readSlice :: BitVector rate -> Index beatsPerBlock -> BitVector bus
          readSlice rateBits beatIdx =
            let chunks :: Vec beatsPerBlock (BitVector bus)
                chunks = bitCoerce rateBits
             in chunks !! beatIdx

          (stAfterAbsorb, roundCntNext, ctrlNext) =
            case ctrl of
              AbsIdle padPending_idle padBlock_idle beatCount_idle
                | haveInput ->
                    let (cap, rateBits) = split stBV :: (BitVector (b - rate), BitVector rate)
                        -- writeSlice now XORs incoming data internally
                        rateBits' = writeSlice rateBits beatCount_idle (sData inp)
                        st' = cap ++# rateBits' :: BitVector b
                        -- Check if this is the last beat BEFORE incrementing (since Index wraps)
                        blockComplete = beatCount_idle == maxBeatCount
                        beatCount' = beatCount_idle + 1
                        padP = lastBeat
                        padB = if lastBeat then padBlockVal else padBlock_idle
                     in if blockComplete
                          then
                            ( st',
                              round0,
                              AbsBusy
                                { roundCnt = round0,
                                  seenTLast = padP,
                                  padPending = padP,
                                  padBlock = padB,
                                  beatCount = 0
                                }
                            )
                          else
                            ( st',
                              round0,
                              AbsIdle
                                { padPending = padP,
                                  padBlock = padB,
                                  beatCount = beatCount'
                                }
                            )
                | padPending_idle ->
                    let (cap, rateBits) = split stBV :: (BitVector (b - rate), BitVector rate)
                        -- writeSlice now XORs padding slice internally
                        padSlice = readSlice padBlock_idle beatCount_idle
                        rateBits' = writeSlice rateBits beatCount_idle padSlice
                        st' = cap ++# rateBits' :: BitVector b
                        -- Check if this is the last beat BEFORE incrementing (since Index wraps)
                        blockComplete = beatCount_idle == maxBeatCount
                        beatCount' = beatCount_idle + 1
                     in if blockComplete
                          then
                            ( st',
                              round0,
                              AbsBusy
                                { roundCnt = round0,
                                  seenTLast = True,
                                  padPending = False,
                                  padBlock = 0,
                                  beatCount = 0
                                }
                            )
                          else
                            ( st',
                              round0,
                              AbsIdle
                                { padPending = True,
                                  padBlock = padBlock_idle,
                                  beatCount = beatCount'
                                }
                            )
                | otherwise -> (stBV, round0, ctrl)
              AbsBusy roundCnt_busy seenTLast_busy padPending_busy padBlock_busy beatCount_busy
                | roundCnt_busy == maxRound ->
                    if seenTLast_busy
                      then
                        ( permOut,  -- CRITICAL: Write back permutation result
                          round0,
                          SqIdle
                            { currentBlock = blockFromSt,
                              digestPending = True,
                              squeezeRem = totalDigestBlocks,
                              beatCount = 0
                            }
                        )
                      else
                        ( permOut,  -- CRITICAL: Write back permutation result
                          round0,
                          AbsIdle
                            { padPending = padPending_busy,
                              padBlock = padBlock_busy,
                              beatCount = 0
                            }
                        )
                | otherwise ->
                    ( stBV,
                      roundCnt_busy + 1,
                      AbsBusy
                        { roundCnt = roundCnt_busy + 1,
                          seenTLast = seenTLast_busy,
                          padPending = padPending_busy,
                          padBlock = padBlock_busy,
                          beatCount = beatCount_busy
                        }
                    )
              SqIdle currentBlock_sq _ squeezeRem_sq beatCount_sq
                | canOutput ->
                    let -- Check if this is the last beat BEFORE incrementing (since Index wraps)
                        blockDone = beatCount_sq == maxBeatCount
                        beatCount' = beatCount_sq + 1
                     in if blockDone && squeezeRem_sq == 0
                          then
                            ( stBV,
                              round0,
                              AbsIdle
                                { padPending = False,
                                  padBlock = 0,
                                  beatCount = 0
                                }
                            )
                          else if blockDone
                            then
                              ( stBV,
                                round0,
                                SqBusy
                                  { roundCnt = round0,
                                    digestPending = False,
                                    squeezeRem = squeezeRem_sq - 1,
                                    beatCount = 0
                                  }
                              )
                            else
                              ( stBV,
                                round0,
                                SqIdle
                                  { currentBlock = currentBlock_sq,
                                    digestPending = True,
                                    squeezeRem = squeezeRem_sq,
                                    beatCount = beatCount'
                                  }
                              )
                | otherwise -> (stBV, round0, ctrl)
              SqBusy roundCnt_sqbusy digestPending_sqbusy squeezeRem_sqbusy beatCount_sqbusy
                | roundCnt_sqbusy == maxRound ->
                    ( permOut,  -- CRITICAL: Write back permutation result
                      round0,
                      SqIdle
                        { currentBlock = blockFromSt,
                          digestPending = True,
                          squeezeRem = squeezeRem_sqbusy,
                          beatCount = 0
                        }
                    )
                | otherwise ->
                    ( stBV,
                      roundCnt_sqbusy + 1,
                      SqBusy
                        { roundCnt = roundCnt_sqbusy + 1,
                          digestPending = digestPending_sqbusy,
                          squeezeRem = squeezeRem_sqbusy,
                          beatCount = beatCount_sqbusy
                        }
                    )

          -- Outputs from CURRENT control state (not ctrlNext)
          -- CRITICAL: AXI handshake must reflect current cycle's state
          out = case ctrl of
            AbsIdle padPending_out _ _ ->
              Out
                { -- Only accept input when not padding
                  -- When padding is pending, we must complete pad block injection first
                  sReady = not padPending_out,
                  mValid = False,
                  mData = 0,
                  mLast = False
                }
            AbsBusy {} -> Out {sReady = False, mValid = False, mData = 0, mLast = False}
            SqIdle currentBlock_out digestPending_out squeezeRem_out beatCount_out ->
              let outputSlice = readSlice currentBlock_out beatCount_out
                  isLastBeat = beatCount_out >= maxBeatCount
               in Out
                    { sReady = False,
                      mValid = digestPending_out,
                      mData = outputSlice,
                      mLast = squeezeRem_out == 0 && isLastBeat
                    }
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
