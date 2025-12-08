{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AXI4Stream
  ( -- * AXI4-Stream Interface
    AXI4Stream (..),
    -- * Common Bus Widths
    AXI4Stream32,
    AXI4Stream64,
    AXI4Stream128,
    -- * Utilities
    idleAXI4Stream,
    validBeat,
    handshake,
  )
where

import Clash.Prelude hiding (tlast)

--------------------------------------------------------------------------------
-- AXI4-Stream Interface
--------------------------------------------------------------------------------

-- | AXI4-Stream master output signals
--
-- = Overview
--
-- This type represents the OUTPUT signals from an AXI4-Stream master:
--
-- * @tdata@  - Data payload (parameterized width)
-- * @tvalid@ - Valid signal (asserted by master when data is available)
-- * @tlast@  - Last beat of packet/transaction
--
-- Note: The @tready@ input signal (from slave to master) is handled
-- separately as a @Signal dom Bool@ parameter, not part of this record.
--
-- = AXI4-Stream Handshake
--
-- A data transfer occurs when both @tvalid@ (from this record) and
-- @tready@ (separate input) are high.
--
-- = Usage
--
-- @
-- -- Define a 64-bit AXI4-Stream output signal
-- type MyStream = AXI4Stream 64
--
-- -- Function that produces AXI4-Stream output and accepts tready input
-- myFunction :: Signal dom Bool -> Signal dom (AXI4Stream 64)
-- @
data AXI4Stream (n :: Nat) = AXI4Stream
  { tdata :: BitVector n, -- ^ Data payload (master output)
    tvalid :: Bool, -- ^ Valid signal (master output)
    tlast :: Bool -- ^ Last beat indicator (master output)
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFDataX)

--------------------------------------------------------------------------------
-- Common Bus Widths
--------------------------------------------------------------------------------

-- | 32-bit AXI4-Stream
type AXI4Stream32 = AXI4Stream 32

-- | 64-bit AXI4-Stream (most common)
type AXI4Stream64 = AXI4Stream 64

-- | 128-bit AXI4-Stream
type AXI4Stream128 = AXI4Stream 128

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Create an idle AXI4-Stream value (all output signals deasserted)
idleAXI4Stream :: (KnownNat n) => AXI4Stream n
idleAXI4Stream =
  AXI4Stream
    { tdata = 0,
      tvalid = False,
      tlast = False
    }

-- | Create a valid data beat
validBeat :: (KnownNat n) => BitVector n -> Bool -> AXI4Stream n
validBeat dat isLast =
  AXI4Stream
    { tdata = dat,
      tvalid = True,
      tlast = isLast
    }

-- | Check if a valid AXI4-Stream handshake occurred
--   Takes tready as a separate parameter since it's an input signal
handshake :: Bool -> AXI4Stream n -> Bool
handshake tready axi = tvalid axi && tready
