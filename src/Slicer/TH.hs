{-# LANGUAGE TemplateHaskell #-}

module Slicer.TH (mkWrite, mkMap) where

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

-- | Generate a map function that applies an operation to a slice of a BitVector
-- Usage: $(mkMap "staticXOR256'" "xor" 1600 [(0, 0, 64), (1, 64, 64), ...])
-- Parameters:
--   funcName  - Name of the generated function
--   opName    - Name of the binary operation (e.g., "xor")
--   stateSize - Total bit width of the state (e.g., 1600)
--   slices    - List of (index, start, laneSize) tuples
-- Generates:
--   staticXOR256' :: BitVector 1600 -> BitVector 64 -> Index 17 -> BitVector 1600
--   staticXOR256' state block 0 = setSlice (SNat @63) (SNat @0) (slice (SNat @63) (SNat @0) state `xor` block) state
--   ...
mkMap :: String -> String -> Integer -> [(Integer, Integer, Integer)] -> Q [Dec]
mkMap funcName opName stateSize slices = do
  let stateName = mkName "state"
      blockName = mkName "block"
      funcNameN = mkName funcName
      opNameN = mkName opName
      setSliceName = mkName "setSlice"
      sliceName = mkName "slice"
      snatName = mkName "SNat"
      bitVectorName = mkName "BitVector"
      indexName = mkName "Index"

      -- Extract laneSize from first tuple (assumed uniform)
      laneSize = case slices of
        ((_, _, ls):_) -> ls
        [] -> error "mkMap: empty slices list"

      numCases = toInteger (length slices)

      -- Build type: BitVector stateSize -> BitVector laneSize -> Index numCases -> BitVector stateSize
      bvState = AppT (ConT bitVectorName) (LitT (NumTyLit stateSize))
      bvLane = AppT (ConT bitVectorName) (LitT (NumTyLit laneSize))
      idxCases = AppT (ConT indexName) (LitT (NumTyLit numCases))
      funcTy = foldr1 (\a b -> AppT (AppT ArrowT a) b) [bvState, bvLane, idxCases, bvState]
      typeSig = SigD funcNameN funcTy

      -- Generate clause for each tuple
      mkClause :: Bool -> (Integer, Integer, Integer) -> Q Clause
      mkClause isLast (idx, start, ls) = do
        let upper = start + ls - 1
            upperTy = LitT (NumTyLit upper)
            lowerTy = LitT (NumTyLit start)
            snatUpper = AppTypeE (ConE snatName) upperTy
            snatLower = AppTypeE (ConE snatName) lowerTy
            -- Pattern: use WildP for last case, LitP for others
            pat = if isLast then WildP else LitP (IntegerL idx)
            -- slice (SNat @upper) (SNat @lower) state
            sliceExpr = foldl AppE (VarE sliceName) [snatUpper, snatLower, VarE stateName]
            -- (slice ... state) `op` block
            opExpr = InfixE (Just sliceExpr) (VarE opNameN) (Just (VarE blockName))
            -- setSlice (SNat @upper) (SNat @lower) (... `op` block) state
            body = foldl AppE (VarE setSliceName) [snatUpper, snatLower, opExpr, VarE stateName]
        pure $ Clause [VarP stateName, VarP blockName, pat] (NormalB body) []

      -- Mark the last element
      taggedSlices = zip (replicate (length slices - 1) False ++ [True]) slices

  clauses <- mapM (uncurry mkClause) taggedSlices
  pure [typeSig, FunD funcNameN clauses]
