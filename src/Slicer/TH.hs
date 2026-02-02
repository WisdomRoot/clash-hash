{-# LANGUAGE TemplateHaskell #-}

module Slicer.TH (mkWrite) where

import Language.Haskell.TH
import Prelude

-- | Generate write function with 25 pattern-matched cases
-- Usage: $(mkWrite)
-- Generates:
--   write :: BitVector 64 -> Index 25 -> BitVector 1600 -> BitVector 1600
--   write lane 0 bv = setSlice (SNat @63) (SNat @0) lane bv
--   ...
mkWrite :: Q [Dec]
mkWrite = do
  let laneName = mkName "lane"
      bvName = mkName "bv"
      writeName = mkName "write"
      setSliceName = mkName "setSlice"
      snatName = mkName "SNat"
      bitVectorName = mkName "BitVector"
      indexName = mkName "Index"

      -- Build type: BitVector 64 -> Index 25 -> BitVector 1600 -> BitVector 1600
      bv64 = AppT (ConT bitVectorName) (LitT (NumTyLit 64))
      idx25 = AppT (ConT indexName) (LitT (NumTyLit 25))
      bv1600 = AppT (ConT bitVectorName) (LitT (NumTyLit 1600))
      writeTy = foldr1 (\a b -> AppT (AppT ArrowT a) b) [bv64, idx25, bv1600, bv1600]
      typeSig = SigD writeName writeTy

      -- Generate clause for index i: write lane i bv = setSlice (SNat @upper) (SNat @lower) lane bv
      mkClause :: Integer -> Q Clause
      mkClause i = do
        let upper = 64 * (i + 1) - 1
            lower = 64 * i
            upperTy = LitT (NumTyLit upper)
            lowerTy = LitT (NumTyLit lower)
            snatUpper = AppTypeE (ConE snatName) upperTy
            snatLower = AppTypeE (ConE snatName) lowerTy
            pat = if i < 24
                  then LitP (IntegerL i)
                  else WildP
            body = foldl AppE (VarE setSliceName) [snatUpper, snatLower, VarE laneName, VarE bvName]
        pure $ Clause [VarP laneName, pat, VarP bvName] (NormalB body) []

  clauses <- mapM mkClause [0..24]
  pure [typeSig, FunD writeName clauses]
