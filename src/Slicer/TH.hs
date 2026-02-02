{-# LANGUAGE TemplateHaskell #-}

module Slicer.TH (mkWrite) where

import Language.Haskell.TH
import Prelude

-- | Generate a customizable write function with pattern-matched cases
-- Usage: $(mkWrite "write" 64 25)
-- Parameters:
--   funcName  - Name of the generated function
--   laneSize  - Bit width of each lane (e.g., 64)
--   numLanes  - Number of lanes (e.g., 25)
-- Generates:
--   write :: BitVector 64 -> Index 25 -> BitVector 1600 -> BitVector 1600
--   write lane 0 bv = setSlice (SNat @63) (SNat @0) lane bv
--   ...
mkWrite :: String -> Integer -> Integer -> Q [Dec]
mkWrite funcName laneSize numLanes = do
  let totalSize = laneSize * numLanes
      laneName = mkName "lane"
      bvName = mkName "bv"
      writeName = mkName funcName
      setSliceName = mkName "setSlice"
      snatName = mkName "SNat"
      bitVectorName = mkName "BitVector"
      indexName = mkName "Index"

      -- Build type: BitVector laneSize -> Index numLanes -> BitVector totalSize -> BitVector totalSize
      bvLane = AppT (ConT bitVectorName) (LitT (NumTyLit laneSize))
      idxLanes = AppT (ConT indexName) (LitT (NumTyLit numLanes))
      bvTotal = AppT (ConT bitVectorName) (LitT (NumTyLit totalSize))
      writeTy = foldr1 (\a b -> AppT (AppT ArrowT a) b) [bvLane, idxLanes, bvTotal, bvTotal]
      typeSig = SigD writeName writeTy

      -- Generate clause for index i: write lane i bv = setSlice (SNat @upper) (SNat @lower) lane bv
      mkClause :: Integer -> Q Clause
      mkClause i = do
        let upper = laneSize * (i + 1) - 1
            lower = laneSize * i
            upperTy = LitT (NumTyLit upper)
            lowerTy = LitT (NumTyLit lower)
            snatUpper = AppTypeE (ConE snatName) upperTy
            snatLower = AppTypeE (ConE snatName) lowerTy
            pat = if i < numLanes - 1
                  then LitP (IntegerL i)
                  else WildP
            body = foldl AppE (VarE setSliceName) [snatUpper, snatLower, VarE laneName, VarE bvName]
        pure $ Clause [VarP laneName, pat, VarP bvName] (NormalB body) []

  clauses <- mapM mkClause [0..numLanes-1]
  pure [typeSig, FunD writeName clauses]
