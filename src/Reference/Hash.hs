module Reference.Hash
  ( sponge,
    sha3_256,
    shake256,
  )
where

import Clash.Prelude hiding (fromList)
import Clash.Sized.Vector qualified as V
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

-- | SHAKE256 using runtime-length input and variable output length
shake256 :: Int -> [Bit] -> [Bit]
shake256 outputBits input =
  let domainSep = [1, 1, 1, 1]  -- SHAKE256 domain separator: 0x1F = "1111"
      inputWithDomain = input P.++ domainSep
   in sponge @1600 @1088 SHA3.keccakf outputBits inputWithDomain
