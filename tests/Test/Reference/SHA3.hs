{-# LANGUAGE ScopedTypeVariables #-}

module Test.Reference.SHA3 (spec) where

import Clash.Prelude hiding (fromList)
import Clash.Sized.Vector qualified as V
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA3_256)
import Data.ByteArray (convert)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Bits qualified as Bits
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.TypeLits (someNatVal, SomeNat (..))
import Prelude (String, ($), Int)
import Prelude qualified as P
import Reference.SHA3 qualified as SHA3
import Test.Hspec

spec :: Spec
spec = describe "Reference SHA3-256 Tests" $ do
  for_ testCases $ \(label, input) ->
    it label $ do
      let cryptonResult = sha3_256_crypton input
          referenceResult = sha3_256_reference input
      referenceResult `shouldBe` cryptonResult

testCases :: [(String, BS8.ByteString)]
testCases =
  [ ("Empty input", BS8.empty),
    ("8-byte input", BS8.pack "qwertyui"),
    ("16-byte input", BS8.pack "qwertyuiopasdfgh"),
    ("Small message", BS8.pack "test"),
    ("Longer message", BS8.pack "The quick brown fox jumps over the lazy dog")
  ]

-- | Compute SHA3-256 using crypton library
sha3_256_crypton :: ByteString -> ByteString
sha3_256_crypton input =
  convert (hash input :: Digest SHA3_256)

-- | Compute SHA3-256 using Reference.SHA3.keccakf (value-level sponge)
sha3_256_reference :: ByteString -> ByteString
sha3_256_reference input =
  let inputBits = byteStringToBitList input
      resultBits = sha3_256_sponge inputBits
   in bitListToByteString resultBits

-- | SHA3-256 sponge construction (value-level implementation)
sha3_256_sponge :: [Bit] -> [Bit]
sha3_256_sponge inputBits =
  let rate = 1088 -- SHA3-256 rate
      capacity = 512 -- SHA3-256 capacity
      stateSize = 1600 -- Keccak-f[1600]

      -- Pad input (SHA3-256 uses domain separator 0x06 = 01)
      paddedInput = padSHA3_256 inputBits rate

      -- Absorb phase
      absorbedState = absorbPhase paddedInput rate stateSize

      -- Squeeze phase (SHA3-256 outputs 256 bits)
      squeezedBits = squeezePhase absorbedState rate 256
   in squeezedBits

-- | Pad input for SHA3-256 (domain separator 0x06, then pad10*1)
padSHA3_256 :: [Bit] -> Int -> [[Bit]]
padSHA3_256 input rate =
  let -- SHA3-256 domain separator: 0x06 = 01 (2 bits)
      domainSep = [0, 1]
      inputWithDomain = input P.++ domainSep
      inputLen = P.length inputWithDomain

      -- Calculate padding needed for pad10*1
      -- Need to pad to multiple of rate
      totalLen = inputLen P.+ 2 -- +2 for the 1...1 padding
      paddingNeeded = rate P.- (totalLen `P.mod` rate)

      -- pad10*1: append 1, then zeros, then 1
      padded = inputWithDomain P.++ [1] P.++ P.replicate paddingNeeded 0 P.++ [1]

      -- Split into rate-sized blocks
      blocks = chunksOf rate padded
   in blocks

-- | Absorb phase: XOR input blocks into state and apply permutation
absorbPhase :: [[Bit]] -> Int -> Int -> Vec 1600 Bit
absorbPhase blocks rate stateSize =
  let initialState = P.replicate 1600 0
   in P.foldl absorbBlock (V.unsafeFromList initialState) blocks
  where
    absorbBlock :: Vec 1600 Bit -> [Bit] -> Vec 1600 Bit
    absorbBlock state block =
      let -- XOR block into first 'rate' bits of state
          stateList = toList state
          paddedBlock = P.take rate (block P.++ P.repeat 0)
          xored = P.zipWith xor (P.take rate stateList) paddedBlock P.++ P.drop rate stateList
          xoredState = V.unsafeFromList xored :: Vec 1600 Bit
          -- Apply Keccak-f[1600]
          permuted = SHA3.keccakf xoredState
       in permuted

-- | Squeeze phase: extract output bits from state
squeezePhase :: Vec 1600 Bit -> Int -> Int -> [Bit]
squeezePhase state rate outputBits =
  let -- Extract 'outputBits' from state (SHA3-256 only needs one squeeze)
      extracted = P.take outputBits (toList state)
   in extracted

-- | Split list into chunks of size n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = P.splitAt n xs
   in chunk : chunksOf n rest

-- Helper functions for bit conversion

-- | Convert ByteString to list of Bits (LSB first for each byte)
byteStringToBitList :: ByteString -> [Bit]
byteStringToBitList bs =
  P.concatMap byteToBits (BS.unpack bs)
  where
    byteToBits :: Word8 -> [Bit]
    byteToBits w = [if Bits.testBit w i then 1 else 0 | i <- [0 .. 7]]

-- | Convert list of Bits to ByteString (LSB first for each byte)
bitListToByteString :: [Bit] -> ByteString
bitListToByteString bits =
  BS.pack (bitsToBytes bits)
  where
    bitsToBytes :: [Bit] -> [Word8]
    bitsToBytes [] = []
    bitsToBytes bs =
      let (chunk, rest) = P.splitAt 8 bs
          paddedChunk = P.take 8 (chunk P.++ P.repeat 0)
          byte = P.foldl accumBit 0 (P.zip [0..] paddedChunk)
       in byte : bitsToBytes rest

    accumBit :: Word8 -> (Int, Bit) -> Word8
    accumBit acc (i, b) = if b P.== 1 then Bits.setBit acc i else acc
