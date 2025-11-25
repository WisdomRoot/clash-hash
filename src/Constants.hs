-- NOTE: if you create polymorphic programs like this
--
--    theta :: forall l w b. (Parameter l w b) => Vec b (Vec 11 (Index b))
--    theta
--       | Just Refl <- sameNat (SNat @l) (SNat @0) = $(TH.theta 0)
--       | Just Refl <- sameNat (SNat @l) (SNat @1) = $(TH.theta 1)
--       | Just Refl <- sameNat (SNat @l) (SNat @2) = $(TH.theta 2)
--       | Just Refl <- sameNat (SNat @l) (SNat @3) = $(TH.theta 3)
--       | Just Refl <- sameNat (SNat @l) (SNat @4) = $(TH.theta 4)
--       | Just Refl <- sameNat (SNat @l) (SNat @5) = $(TH.theta 5)
--       | Just Refl <- sameNat (SNat @l) (SNat @6) = $(TH.theta 6)
--       | otherwise = errorX "theta: unsupported lane parameter"
--
-- then Clash will try to synthesize ALL branches of the function
-- even if only one branch is ever taken at runtime!
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Constants
  ( iota,
    chi,
    pi,
    rho,
    theta,
  )
where

import Clash.Prelude (Bit, BitVector, Bits (..), Index, Vec (Nil, (:>)), head, ifoldl, iterateI, last, listToVecTH, repeat, replace, toList, unconcatI, unfoldrI, xor, zipWith, (.&.), (+>>))
import qualified Constants.Indices as Indices
import Language.Haskell.TH
import Prelude hiding (head, last, pi, repeat, zipWith, (!!))

-- | All 24 round constants for Keccak-f, 64 bits each.
--
-- The constants are generated at compile time via Template Haskell.
-- This exactly matches SHA3internal._iota_constants logic.
iota :: Q Exp
iota = [| iotaConstants |]
  where
    iotaConstants :: Vec 24 (Vec 64 Bit)
    iotaConstants = fmap (ifoldl g (repeat 0)) lfsr
      where
        -- This matches: lfsr = unconcatI . unfoldrI f $ bv2v $(bLit "10000000") :: Vec 24 (Vec 7 Bit)
        lfsr :: Vec 24 (Vec 7 Bit)
        lfsr = unconcatI (unfoldrI f (1 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil))

        -- f t = (head t, zipWith xor (0 +>> t) . fmap (last t .&.) $ bv2v $(bLit "10001110"))
        f :: Vec 8 Bit -> (Bit, Vec 8 Bit)
        f t =
          let shifted = 0 +>> t
              mask = 1 :> 0 :> 0 :> 0 :> 1 :> 1 :> 1 :> 0 :> Nil  -- bv2v $(bLit "10001110") LSB first
              feedback = last t
              next = zipWith (\a m -> a `xor` (feedback .&. m)) shifted mask
          in (head t, next)

        -- g t j b = replace @_ @(Unsigned 7) (2 P.^ j - 1) b t
        g :: Vec 64 Bit -> Index 7 -> Bit -> Vec 64 Bit
        g t j b = replace ((2 ^ (fromIntegral j :: Integer)) - 1) b t

-- iota :: Index 24 -> BitVector 8
-- iota idx = truncateB ($(TH.iota) !! idx)

-- | Template Haskell generator for Chi transformation index triples.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- @Vec (25*w) (Index (25*w), Index (25*w), Index (25*w))@.
chi :: Int -> Q Exp
chi l = do
  let triples = Indices.chi l
      b = 25 * (2 ^ l)

      idxType = mkIndexType b
      mkIndex = mkIndexLit idxType

      mkTriple (i0, i1, i2) =
        TupE [Just (mkIndex i0), Just (mkIndex i1), Just (mkIndex i2)]

      tripleType = AppT (AppT (AppT (ConT ''(,,)) idxType) idxType) idxType

  pure (mkVecLiteral b tripleType (map mkTriple triples))

-- | Template Haskell generator for Pi transformation permutation.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- @Vec (25*w) (Index (25*w))@.
pi :: Int -> Q Exp
pi = mkPermutation Indices.pi

-- | Template Haskell generator for Rho transformation permutation.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- @Vec (25*w) (Index (25*w))@.
rho :: Int -> Q Exp
rho = mkPermutation Indices.rho

-- | Template Haskell generator for Theta transformation index lookup.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- @Vec (25*w) (Vec 11 (Index (25*w)))@.
theta :: Int -> Q Exp
theta l = do
  let allIndices = Indices.theta l
      b = stateSize l

      idxType = mkIndexType b
      vec11Type = mkVecType 11 idxType

      mkIndex = mkIndexLit idxType
      mkInner row = mkVecLiteral 11 idxType (map mkIndex row)

  pure (mkVecLiteral b vec11Type (map mkInner allIndices))

-- Helpers -----------------------------------------------------------------

mkPermutation :: (Int -> [Int]) -> Int -> Q Exp
mkPermutation gen l = do
  let srcIndices = gen l
      b = stateSize l

      idxType = mkIndexType b
      mkIndex = mkIndexLit idxType

  pure (mkVecLiteral b idxType (map mkIndex srcIndices))

stateSize :: Int -> Int
stateSize l = 25 * (2 ^ l)

mkIndexType :: Int -> Type
mkIndexType = AppT (ConT ''Index) . natLit

mkVecType :: Int -> Type -> Type
mkVecType len = AppT (AppT (ConT ''Vec) (natLit len))

mkIndexLit :: Type -> Int -> Exp
mkIndexLit idxType n = SigE (LitE (IntegerL (fromIntegral n))) idxType

mkVecLiteral :: Int -> Type -> [Exp] -> Exp
mkVecLiteral len elemType elems =
  let cons x xs = InfixE (Just x) (ConE '(:>)) (Just xs)
      body = foldr cons (ConE 'Nil) elems
   in SigE body (mkVecType len elemType)

natLit :: Int -> Type
natLit = LitT . NumTyLit . fromIntegral
