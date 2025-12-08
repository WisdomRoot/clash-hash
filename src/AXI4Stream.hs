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
    readyAXI4Stream,
    handshake,
  )
where

import Clash.Prelude hiding (tlast)

--------------------------------------------------------------------------------
-- AXI4-Stream Interface
--------------------------------------------------------------------------------

-- | AXI4-Stream interface datatype
--
-- = Overview
--
-- Minimal AXI4-Stream interface with 4 signals:
--
-- * @tdata@  - Data payload (parameterized width)
-- * @tvalid@ - Valid signal (asserted by master)
-- * @tready@ - Ready signal (asserted by slave)
-- * @tlast@  - Last beat of packet/transaction
--
-- = AXI4-Stream Handshake
--
-- A data transfer occurs when both @tvalid@ and @tready@ are high.
-- The master asserts @tvalid@ when data is available.
-- The slave asserts @tready@ when it can accept data.
--
-- = Usage
--
-- @
-- -- Define a 64-bit AXI4-Stream signal
-- type MyStream = AXI4Stream 64
--
-- -- Or use predefined aliases
-- type MyStream = AXI4Stream64
-- @
data AXI4Stream (n :: Nat) = AXI4Stream
  { tdata :: BitVector n, -- ^ Data payload
    tvalid :: Bool, -- ^ Valid signal (master)
    tready :: Bool, -- ^ Ready signal (slave)
    tlast :: Bool -- ^ Last beat indicator
  }
  deriving stock (Generic)
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

-- | Create an idle AXI4-Stream value (all signals deasserted)
idleAXI4Stream :: (KnownNat n) => AXI4Stream n
idleAXI4Stream =
  AXI4Stream
    { tdata = 0,
      tvalid = False,
      tready = False,
      tlast = False
    }

-- | Create a valid data beat
validBeat :: (KnownNat n) => BitVector n -> Bool -> AXI4Stream n
validBeat dat isLast =
  AXI4Stream
    { tdata = dat,
      tvalid = True,
      tready = False, -- Ready is controlled by slave
      tlast = isLast
    }

-- | Create a ready AXI4-Stream value (slave ready to receive)
readyAXI4Stream :: (KnownNat n) => AXI4Stream n
readyAXI4Stream =
  AXI4Stream
    { tdata = 0,
      tvalid = False,
      tready = True,
      tlast = False
    }

-- | Check if a valid AXI4-Stream handshake occurred
handshake :: AXI4Stream n -> Bool
handshake axi = tvalid axi && tready axi
