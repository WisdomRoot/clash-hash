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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Constants
  ( iota,
    chi,
    pi,
    rho,
    theta,
  )
where

import Clash.Prelude (BitVector, Index, Vec (Nil, (:>)), truncateB, (!!))
import qualified Constants.Indices as Indices
import qualified Constants.TH as TH
import Language.Haskell.TH
import Prelude hiding (pi, (!!))

-- | All 24 round constants for Keccak-f, 64 bits each.
--
-- The constants are generated at compile time via Template Haskell so that
-- synthesized hardware sees a literal Vec instead of rebuilding the LFSR.
iota :: Index 24 -> BitVector 8
iota idx = truncateB ($(TH.iota) !! idx)

-- | Chi transformation index lookup table for Keccak-f[200].
--
-- Contains 200 index triples (i0, i1, i2) where:
--   output[idx] = state[i0] ⊕ (¬state[i1] ∧ state[i2])
-- Generated at compile time via Template Haskell.
chi :: Vec 200 (Index 200, Index 200, Index 200)
chi = $(TH.chi)

-- | Pi transformation permutation table for Keccak-f[200].
--
-- Contains 200 source indices for the pi permutation.
-- Pi formula: (i, j, k) -> (j, 3*i + j, k)
-- Generated at compile time via Template Haskell.
pi :: Vec 200 (Index 200)
pi = $(TH.pi)

-- | Template Haskell generator for Rho transformation permutation.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- @Vec (25*w) (Index (25*w))@.
rho :: Int -> Q Exp
rho l = do
  let srcIndices = Indices.rho l
      b = 25 * (2 ^ l)

      idxType = AppT (ConT ''Index) (LitT (NumTyLit (fromIntegral b)))

      mkIndex n =
        SigE (LitE (IntegerL (fromIntegral n))) idxType

      mkVec :: Int -> Type -> [Exp] -> Exp
      mkVec len elemType elems =
        let cons x xs = InfixE (Just x) (ConE '(:>)) (Just xs)
            body = foldr cons (ConE 'Nil) elems
            vecType = AppT (AppT (ConT ''Vec) (LitT (NumTyLit (fromIntegral len)))) elemType
         in SigE body vecType

  pure (mkVec b idxType (map mkIndex srcIndices))

-- | Template Haskell generator for Theta transformation index lookup.
-- Takes Keccak parameter @l@ (lane width w = 2^l) and returns
-- @Vec (25*w) (Vec 11 (Index (25*w)))@.
theta :: Int -> Q Exp
theta l = do
  let allIndices = Indices.theta l
      b = 25 * (2 ^ l)

      idxType = AppT (ConT ''Index) (LitT (NumTyLit (fromIntegral b)))
      vec11Type = AppT (AppT (ConT ''Vec) (LitT (NumTyLit 11))) idxType

      mkIndex n =
        SigE (LitE (IntegerL (fromIntegral n))) idxType

      mkVec :: Int -> Type -> [Exp] -> Exp
      mkVec len elemType elems =
        let cons x xs = InfixE (Just x) (ConE '(:>)) (Just xs)
            body = foldr cons (ConE 'Nil) elems
            vecType = AppT (AppT (ConT ''Vec) (LitT (NumTyLit (fromIntegral len)))) elemType
         in SigE body vecType

      mkInner row = mkVec 11 idxType (map mkIndex row)
      mkOuter rows = mkVec b vec11Type (map mkInner rows)

  pure (mkOuter allIndices)
