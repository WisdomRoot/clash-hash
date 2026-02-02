{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Slicer
  ( read,
    write,
    map,
  )
where

import Clash.Prelude hiding (map, read)
import Slicer.TH (mkWrite)

-- | Read one of the 25 64-bit lanes from a 1600-bit state
read :: Index 25 -> BitVector 1600 -> BitVector 64
read 0 bv = slice (SNat @63) (SNat @0) bv
read 1 bv = slice (SNat @127) (SNat @64) bv
read 2 bv = slice (SNat @191) (SNat @128) bv
read 3 bv = slice (SNat @255) (SNat @192) bv
read 4 bv = slice (SNat @319) (SNat @256) bv
read 5 bv = slice (SNat @383) (SNat @320) bv
read 6 bv = slice (SNat @447) (SNat @384) bv
read 7 bv = slice (SNat @511) (SNat @448) bv
read 8 bv = slice (SNat @575) (SNat @512) bv
read 9 bv = slice (SNat @639) (SNat @576) bv
read 10 bv = slice (SNat @703) (SNat @640) bv
read 11 bv = slice (SNat @767) (SNat @704) bv
read 12 bv = slice (SNat @831) (SNat @768) bv
read 13 bv = slice (SNat @895) (SNat @832) bv
read 14 bv = slice (SNat @959) (SNat @896) bv
read 15 bv = slice (SNat @1023) (SNat @960) bv
read 16 bv = slice (SNat @1087) (SNat @1024) bv
read 17 bv = slice (SNat @1151) (SNat @1088) bv
read 18 bv = slice (SNat @1215) (SNat @1152) bv
read 19 bv = slice (SNat @1279) (SNat @1216) bv
read 20 bv = slice (SNat @1343) (SNat @1280) bv
read 21 bv = slice (SNat @1407) (SNat @1344) bv
read 22 bv = slice (SNat @1471) (SNat @1408) bv
read 23 bv = slice (SNat @1535) (SNat @1472) bv
read _ bv = slice (SNat @1599) (SNat @1536) bv

-- | Write a 64-bit lane into a 1600-bit state at the given index
-- Generated via Template Haskell
$(mkWrite)

-- | Apply a function to a specific 64-bit lane
map :: (BitVector 64 -> BitVector 64) -> Index 25 -> BitVector 1600 -> BitVector 1600
map f 0 bv = setSlice (SNat @63) (SNat @0) (f (slice (SNat @63) (SNat @0) bv)) bv
map f 1 bv = setSlice (SNat @127) (SNat @64) (f (slice (SNat @127) (SNat @64) bv)) bv
map f 2 bv = setSlice (SNat @191) (SNat @128) (f (slice (SNat @191) (SNat @128) bv)) bv
map f 3 bv = setSlice (SNat @255) (SNat @192) (f (slice (SNat @255) (SNat @192) bv)) bv
map f 4 bv = setSlice (SNat @319) (SNat @256) (f (slice (SNat @319) (SNat @256) bv)) bv
map f 5 bv = setSlice (SNat @383) (SNat @320) (f (slice (SNat @383) (SNat @320) bv)) bv
map f 6 bv = setSlice (SNat @447) (SNat @384) (f (slice (SNat @447) (SNat @384) bv)) bv
map f 7 bv = setSlice (SNat @511) (SNat @448) (f (slice (SNat @511) (SNat @448) bv)) bv
map f 8 bv = setSlice (SNat @575) (SNat @512) (f (slice (SNat @575) (SNat @512) bv)) bv
map f 9 bv = setSlice (SNat @639) (SNat @576) (f (slice (SNat @639) (SNat @576) bv)) bv
map f 10 bv = setSlice (SNat @703) (SNat @640) (f (slice (SNat @703) (SNat @640) bv)) bv
map f 11 bv = setSlice (SNat @767) (SNat @704) (f (slice (SNat @767) (SNat @704) bv)) bv
map f 12 bv = setSlice (SNat @831) (SNat @768) (f (slice (SNat @831) (SNat @768) bv)) bv
map f 13 bv = setSlice (SNat @895) (SNat @832) (f (slice (SNat @895) (SNat @832) bv)) bv
map f 14 bv = setSlice (SNat @959) (SNat @896) (f (slice (SNat @959) (SNat @896) bv)) bv
map f 15 bv = setSlice (SNat @1023) (SNat @960) (f (slice (SNat @1023) (SNat @960) bv)) bv
map f 16 bv = setSlice (SNat @1087) (SNat @1024) (f (slice (SNat @1087) (SNat @1024) bv)) bv
map f 17 bv = setSlice (SNat @1151) (SNat @1088) (f (slice (SNat @1151) (SNat @1088) bv)) bv
map f 18 bv = setSlice (SNat @1215) (SNat @1152) (f (slice (SNat @1215) (SNat @1152) bv)) bv
map f 19 bv = setSlice (SNat @1279) (SNat @1216) (f (slice (SNat @1279) (SNat @1216) bv)) bv
map f 20 bv = setSlice (SNat @1343) (SNat @1280) (f (slice (SNat @1343) (SNat @1280) bv)) bv
map f 21 bv = setSlice (SNat @1407) (SNat @1344) (f (slice (SNat @1407) (SNat @1344) bv)) bv
map f 22 bv = setSlice (SNat @1471) (SNat @1408) (f (slice (SNat @1471) (SNat @1408) bv)) bv
map f 23 bv = setSlice (SNat @1535) (SNat @1472) (f (slice (SNat @1535) (SNat @1472) bv)) bv
map f _ bv = setSlice (SNat @1599) (SNat @1536) (f (slice (SNat @1599) (SNat @1536) bv)) bv
