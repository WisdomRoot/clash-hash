module Reference.Hash
  ( sponge,
    sha3_256,
    sha3_256BS,
    shake256,
    shake256BS,
    bsToBitList,
    bitListToBS,
  )
where

import Clash.Prelude hiding (fromList)
import Clash.Sized.Vector qualified as V
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Prelude qualified as P
import Reference.SHA3 qualified as SHA3

sponge ::
  forall b r.
  ( KnownNat b,
    KnownNat r,
    1 <= b,
    1 <= r,
    r <= b
  ) =>
  (Vec b Bit -> Vec b Bit) ->
  Int ->
  [Bit] ->
  [Bit]
sponge f outputBits = trunc . squeeze . absorb . pad
  where
    rate = natToNum @r

    pad :: [Bit] -> [[Bit]]
    pad input =
      let inputLen = P.length input
          totalLen = inputLen P.+ 2
          paddingNeeded = rate P.- (totalLen `P.mod` rate)
          padded = input P.++ [1] P.++ P.replicate paddingNeeded 0 P.++ [1]
       in chunksOf rate padded

    absorb :: [[Bit]] -> Vec b Bit
    absorb = P.foldl g (repeat 0)
      where
        g :: Vec b Bit -> [Bit] -> Vec b Bit
        g s chunk =
          let chunkVec = V.unsafeFromList (P.take rate (chunk P.++ P.repeat 0)) :: Vec r Bit
              blockPre = zipWith xor s (chunkVec ++ repeat @(b - r) 0)
              permuted = f blockPre
           in permuted

    squeeze :: Vec b Bit -> [[Bit]]
    squeeze state = go state []
      where
        go s acc
          | P.length (P.concat acc) P.>= outputBits = acc
          | otherwise =
              let extracted = P.take rate (toList s)
                  permuted = f s
               in go permuted (acc P.++ [extracted])

    trunc :: [[Bit]] -> [Bit]
    trunc blocks = P.take outputBits (P.concat blocks)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = P.splitAt n xs
   in chunk : chunksOf n rest

-- | SHA3-256 using runtime-length input
sha3_256 :: [Bit] -> Vec 256 Bit
sha3_256 input =
  let domainSep = [0, 1]  -- SHA3-256 domain separator: 0x06 = "01"
      inputWithDomain = input P.++ domainSep
      resultBits = sponge @1600 @1088 SHA3.keccakf 256 inputWithDomain
   in V.unsafeFromList resultBits

-- | SHA3-256 using ByteString input and ByteString output
sha3_256BS :: ByteString -> ByteString
sha3_256BS input =
  let inputBits = bsToBitList input
      resultBits = V.toList (sha3_256 inputBits)
   in bitListToBS resultBits

-- | SHAKE256 using runtime-length input and variable output length
shake256 :: Int -> [Bit] -> [Bit]
shake256 outputBits input =
  let domainSep = [1, 1, 1, 1]  -- SHAKE256 domain separator: 0x1F = "1111"
      inputWithDomain = input P.++ domainSep
   in sponge @1600 @1088 SHA3.keccakf outputBits inputWithDomain

-- | SHAKE256 using ByteString input and ByteString output
shake256BS :: P.Int -> ByteString -> ByteString
shake256BS outputBytes input =
  let inputBits = bsToBitList input
      outputBits = outputBytes P.* 8
      resultBits = shake256 outputBits inputBits
   in bitListToBS resultBits

-- | ByteString -> [Bit] using bitCoerce (LSB-first per byte)
bsToBitList :: ByteString -> [Bit]
bsToBitList bs =
  let bytes = BS.unpack bs
      -- Convert each Word8 to BitVector 8, then toList to [Bit]
      toBits :: Word8 -> [Bit]
      toBits w =
        let bv = bitCoerce w :: BitVector 8
         in P.reverse (V.toList (unpack bv :: Vec 8 Bit))
   in P.concatMap toBits bytes

-- | [Bit] -> ByteString using bitCoerce (LSB-first per byte)
bitListToBS :: [Bit] -> ByteString
bitListToBS bits =
  let chunks = chunksOf 8 bits
      -- Convert each [Bit] chunk to Word8 via BitVector 8
      toWord8 :: [Bit] -> Word8
      toWord8 chunk =
        let paddedChunk = P.take 8 (chunk P.++ P.repeat low)
            vec = V.unsafeFromList (P.reverse paddedChunk) :: Vec 8 Bit
            bv = pack vec :: BitVector 8
         in bitCoerce bv
      bytes = P.map toWord8 chunks
   in BS.pack bytes
