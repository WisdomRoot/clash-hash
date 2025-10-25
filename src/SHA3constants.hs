module SHA3constants
  ( rho_constant
  , iota_constant
  ) where

import qualified Prelude as P
import Clash.Prelude
import Data.Modular

--

rho_constant :: Vec 5 (Vec 5 Integer)
rho_constant = unconcatI $ 0 :> (snd . unzip . sort $ unfoldrI f (0, 0 :: Integer/5, 1, 1)) where
  compareSwap a b = if fst a > fst b then (a,b) else (b,a)
  insert y xs = let (y',xs') = mapAccumL compareSwap y xs in xs' :< y'
  sort = vfold $ const insert
  f (t, i, j, k) = ((5*(unMod i) + unMod j, k), (t + 1, 3*i + 2*j, i, k * (t + 3) `div` (t + 1)))

iota_constant :: Vec 24 (Vec 64 Bit)
iota_constant = fmap (ifoldl g $ repeat 0) lfsr where
  lfsr = unconcatI . unfoldrI f $ bv2v $(bLit "10000000") :: Vec 24 (Vec 7 Bit)
  f t = (head t, zipWith xor (0 +>> t) . fmap (last t .&.) $ bv2v $(bLit "10001110"))
  g t j b = replace @_ @(Unsigned 7) (2 P.^ j - 1) b t
