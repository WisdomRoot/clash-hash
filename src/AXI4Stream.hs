{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AXI4Stream
  ( AXI4Stream (..),
    AXI4Stream32,
    AXI4Stream64,
    AXI4Stream128,
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
  deriving stock (Generic, Show)
  deriving anyclass (NFDataX)

instance Bundle (AXI4Stream n)

--------------------------------------------------------------------------------
-- Common Bus Widths
--------------------------------------------------------------------------------

type AXI4Stream32 = AXI4Stream 32
type AXI4Stream64 = AXI4Stream 64
type AXI4Stream128 = AXI4Stream 128

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
