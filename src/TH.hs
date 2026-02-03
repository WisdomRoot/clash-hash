module TH (mkWrite, mkRead, mkMap) where

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
      writeTy = foldr1 (AppT . AppT ArrowT) [bvLane, idxLanes, bvTotal, bvTotal]
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
            pat =
              if i < numLanes - 1
                then LitP (IntegerL i)
                else WildP
            body = foldl AppE (VarE setSliceName) [snatUpper, snatLower, VarE laneName, VarE bvName]
        pure $ Clause [VarP laneName, pat, VarP bvName] (NormalB body) []

  clauses <- mapM mkClause [0 .. numLanes - 1]
  pure [typeSig, FunD writeName clauses]

-- | Generate a read function that slices out a lane from a BitVector.
--
-- Parameters:
--   funcName  - Name of the generated function
--   stateSize - Total bit width of the state (e.g., 1600)
--   slices    - List of (index, start, laneSize) tuples
--
-- Example (3 lanes, normal order):
--
--   $(mkRead "readNormal" 192 [(0, 0, 64), (1, 64, 64), (2, 128, 64)])
--
--   -- Generates:
--   readNormal :: BitVector 192 -> Index 3 -> BitVector 64
--   readNormal state 0 = slice (SNat @63) (SNat @0) state
--   readNormal state 1 = slice (SNat @127) (SNat @64) state
--   readNormal state 2 = slice (SNat @191) (SNat @128) state
--
-- Example (3 lanes, reversed order):
--
--   $(mkRead "readReversed" 192 [(0, 128, 64), (1, 64, 64), (2, 0, 64)])
--
--   -- Generates:
--   readReversed :: BitVector 192 -> Index 3 -> BitVector 64
--   readReversed state 0 = slice (SNat @191) (SNat @128) state
--   readReversed state 1 = slice (SNat @127) (SNat @64) state
--   readReversed state 2 = slice (SNat @63) (SNat @0) state
--
-- NOTE: The slices list should cover all indices [0..n-1] for Index n.
mkRead :: String -> Integer -> [(Integer, Integer, Integer)] -> Q [Dec]
mkRead funcName stateSize slices = do
  let stateName = mkName "state"
      funcNameN = mkName funcName
      sliceName = mkName "slice"
      snatName = mkName "SNat"
      bitVectorName = mkName "BitVector"
      indexName = mkName "Index"

      laneSize = case slices of
        ((_, _, ls) : _) -> ls
        [] -> error "mkRead: empty slices list"

      numCases = toInteger (length slices)

      -- Build type: BitVector stateSize -> Index numCases -> BitVector laneSize
      bvState = AppT (ConT bitVectorName) (LitT (NumTyLit stateSize))
      bvLane = AppT (ConT bitVectorName) (LitT (NumTyLit laneSize))
      idxCases = AppT (ConT indexName) (LitT (NumTyLit numCases))
      funcTy = foldr1 (AppT . AppT ArrowT) [bvState, idxCases, bvLane]
      typeSig = SigD funcNameN funcTy

      mkClause :: (Integer, Integer, Integer) -> Clause
      mkClause (idx, start, ls) =
        let upper = start + ls - 1
            upperTy = LitT (NumTyLit upper)
            lowerTy = LitT (NumTyLit start)
            snatUpper = AppTypeE (ConE snatName) upperTy
            snatLower = AppTypeE (ConE snatName) lowerTy
            pat = LitP (IntegerL idx)
            body = foldl AppE (VarE sliceName) [snatUpper, snatLower, VarE stateName]
         in Clause [VarP stateName, pat] (NormalB body) []

      allClauses = map mkClause slices

  pure [typeSig, FunD funcNameN allClauses]

-- | Generate a map function that applies an operation to a slice of a BitVector
--
-- Parameters:
--   funcName  - Name of the generated function
--   opName    - Name of the binary operation (e.g., "xor")
--   stateSize - Total bit width of the state (e.g., 1600)
--   slices    - List of (index, start, laneSize) tuples
--
-- Example 1: Normal bit order (3 lanes for brevity)
--
--   $(mkMap "xorNormal" "xor" 192 [(i, i * 64, 64) | i <- [0 .. 2]])
--
--   -- Generates:
--   xorNormal :: BitVector 192 -> BitVector 64 -> Index 3 -> BitVector 192
--   xorNormal state block 0 = setSlice (SNat @63) (SNat @0) (slice (SNat @63) (SNat @0) state `xor` block) state
--   xorNormal state block 1 = setSlice (SNat @127) (SNat @64) (slice (SNat @127) (SNat @64) state `xor` block) state
--   xorNormal state block 2 = setSlice (SNat @191) (SNat @128) (slice (SNat @191) (SNat @128) state `xor` block) state
--   xorNormal state _ _ = state
--
-- Example 2: Reversed bit order (3 lanes for brevity)
--
--   $(mkMap "xorReversed" "xor" 192 [(i, 128 - (i * 64), 64) | i <- [0 .. 2]])
--
--   -- Generates:
--   xorReversed :: BitVector 192 -> BitVector 64 -> Index 3 -> BitVector 192
--   xorReversed state block 0 = setSlice (SNat @191) (SNat @128) (slice (SNat @191) (SNat @128) state `xor` block) state
--   xorReversed state block 1 = setSlice (SNat @127) (SNat @64) (slice (SNat @127) (SNat @64) state `xor` block) state
--   xorReversed state block 2 = setSlice (SNat @63) (SNat @0) (slice (SNat @63) (SNat @0) state `xor` block) state
--   xorReversed state _ _ = state
--
-- IMPORTANT - Synthesis consideration (verified with bench N256N):
--
--   Must have explicit "_ -> state" wildcard fallback that returns state UNCHANGED.
--   BAD:  making the last index use WildP while still applying the operation
--   GOOD: all indices use LitP, then add separate "_ -> state" at the end
--   Reason: The wildcard fallback must return state unchanged, not apply the operation.
--           Getting this wrong causes ~3% area bloat (25574 -> 26332 µm²).
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
        ((_, _, ls) : _) -> ls
        [] -> error "mkMap: empty slices list"

      numCases = toInteger (length slices)

      -- Build type: BitVector stateSize -> BitVector laneSize -> Index numCases -> BitVector stateSize
      bvState = AppT (ConT bitVectorName) (LitT (NumTyLit stateSize))
      bvLane = AppT (ConT bitVectorName) (LitT (NumTyLit laneSize))
      idxCases = AppT (ConT indexName) (LitT (NumTyLit numCases))
      funcTy = foldr1 (AppT . AppT ArrowT) [bvState, bvLane, idxCases, bvState]
      typeSig = SigD funcNameN funcTy

      -- Generate a function clause for each tuple
      mkClause :: (Integer, Integer, Integer) -> Clause
      mkClause (idx, start, ls) =
        let upper = start + ls - 1
            upperTy = LitT (NumTyLit upper)
            lowerTy = LitT (NumTyLit start)
            snatUpper = AppTypeE (ConE snatName) upperTy
            snatLower = AppTypeE (ConE snatName) lowerTy
            pat = LitP (IntegerL idx)
            -- slice (SNat @upper) (SNat @lower) state
            sliceExpr = foldl AppE (VarE sliceName) [snatUpper, snatLower, VarE stateName]
            -- (slice ... state) `op` block
            opExpr = InfixE (Just sliceExpr) (VarE opNameN) (Just (VarE blockName))
            -- setSlice (SNat @upper) (SNat @lower) (... `op` block) state
            body = foldl AppE (VarE setSliceName) [snatUpper, snatLower, opExpr, VarE stateName]
         in Clause [VarP stateName, VarP blockName, pat] (NormalB body) []

      -- Wildcard fallback clause: func state _ _ = state
      wildcardClause = Clause [VarP stateName, WildP, WildP] (NormalB (VarE stateName)) []

      allClauses = map mkClause slices ++ [wildcardClause]

  pure [typeSig, FunD funcNameN allClauses]
