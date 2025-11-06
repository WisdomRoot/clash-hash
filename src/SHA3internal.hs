module SHA3internal
  ( BitString,
    bs2v,
    v2bs,
    hexdump,
    toBitString,
    KeccakParameter,
    State,
    SHA3Constants (..),
    sha3_constants,
    theta,
    rho,
    pi,
    chi,
    iota,
  )
where

import Clash.Prelude hiding (pi)
import Data.Modular
import Data.Proxy (Proxy (..))
import Text.Printf (printf)
import qualified Prelude as P

-- $setup
-- >>> import Clash.Prelude hiding (pi)

type BitString n = Vec n Bit

bs2v :: (KnownNat n, BitPack a) => BitString (n * BitSize a) -> Vec n a
bs2v = map (unpack . v2bv . reverse) . unconcatI

v2bs :: (KnownNat n, BitPack a) => Vec n a -> BitString (n * BitSize a)
v2bs = concat . map (reverse . bv2v . pack)

hexdump :: (KnownNat n) => String -> BitString (8 * n) -> String
hexdump fmt = foldl (P.++) "" . map (printf fmt) . bs2v @_ @(Unsigned 8)

toBitString :: forall n. (KnownNat n) => Vec n Char -> BitString (8 * n)
toBitString = v2bs @_ @(Unsigned 8) . map (fromIntegral . fromEnum)

--

type KeccakParameter l w b =
  ( KnownNat l,
    KnownNat w,
    KnownNat b,
    0 <= l, -- Changed from 3 to allow Keccak-f[25], [50], [100]
    l <= 6,
    1 <= w,
    1 <= b,
    -- , 1 <= b `Div` 8  -- Removed to allow non-byte-aligned sizes
    w ~ 2 ^ l,
    w <= 64,
    b ~ 25 * w,
    b ~ 5 * (5 * w)
    -- , b ~ (b `Div` 8) * 8  -- Removed to allow non-byte-aligned
  )

type State b = BitString b

type Index3 w b = ((Index (2 * b)) / 5, (Index (2 * b)) / 5, (Index (2 * b)) / w)

flatten :: forall l w b. (KeccakParameter l w b) => Index3 w b -> Index b
flatten (i, j, k) = resize $ unMod i * (5 * w) + unMod j * w + unMod k
  where
    w = fromInteger . natVal $ Proxy @w

erect :: forall l w b. (KeccakParameter l w b) => Index b -> Index3 w b
erect idx = (toMod i, toMod j, toMod k)
  where
    w = fromInteger . natVal $ Proxy @w
    (i, r) = resize idx `divMod` (5 * w)
    (j, k) = r `divMod` w

--

data SHA3Constants l w b = SHA3Constants
  { theta_constants :: Vec b (Vec 11 (Index b)),
    rho_constants :: Vec b (Index b),
    pi_constants :: Vec b (Index b),
    chi_constants :: Vec b (Index b, Index b, Index b),
    iota_constants :: Vec 24 (Vec 64 Bit)
  }
  deriving (Lift)

sha3_constants :: (KeccakParameter l w b) => SHA3Constants l w b
sha3_constants = SHA3Constants _theta_constants _rho_constants _pi_constants _chi_constants _iota_constants

_theta_constants :: (KeccakParameter l w b) => Vec b (Vec 11 (Index b))
_theta_constants = fmap (fmap flatten . f . erect) indicesI
  where
    f (i, j, k) =
      (i, j, k)
        :> (0, j - 1, k)
        :> (1, j - 1, k)
        :> (2, j - 1, k)
        :> (3, j - 1, k)
        :> (4, j - 1, k)
        :> (0, j + 1, k - 1)
        :> (1, j + 1, k - 1)
        :> (2, j + 1, k - 1)
        :> (3, j + 1, k - 1)
        :> (4, j + 1, k - 1)
        :> Nil

_rho_constants :: forall l w b. (KeccakParameter l w b) => Vec b (Index b)
_rho_constants = fmap (resize . flatten . f . erect) indicesI
  where
    f (i, j, k) = (i, j, k - unconcatI @5 @5 r !! unMod i !! unMod j)
    r = 0 :> (fmap fromInteger . snd . unzip . sort $ unfoldrI g (0, 0 :: Integer / 5, 1, 1))
    g (t, i, j, k) = ((5 * (unMod i) + unMod j, k), (t + 1, 3 * i + 2 * j, i, k * (t + 3) `div` (t + 1)))
    sort = vfold $ const insert
    insert y xs = let (y', xs') = mapAccumL compareSwap y xs in xs' :< y'
    compareSwap a b = if fst a > fst b then (a, b) else (b, a)

_pi_constants :: forall l w b. (KeccakParameter l w b) => Vec b (Index b)
_pi_constants = fmap (resize . flatten . f . erect) indicesI where f (i, j, k) = (j, 3 * i + j, k)

_chi_constants :: forall l w b. (KeccakParameter l w b) => Vec b (Index b, Index b, Index b)
_chi_constants = fmap (g . erect) indicesI
  where
    g (i, j, k) = let f = resize . flatten
                  in (f (i, j, k), f (i, j + 1, k), f (i, j + 2, k))

_iota_constants :: Vec 24 (Vec 64 Bit)
_iota_constants = fmap (ifoldl g $ repeat 0) lfsr
  where
    lfsr = unconcatI . unfoldrI f $ bv2v $(bLit "10000000") :: Vec 24 (Vec 7 Bit)
    f t = (head t, zipWith xor (0 +>> t) . fmap (last t .&.) $ bv2v $(bLit "10001110"))
    g t j b = replace @_ @(Unsigned 7) (2 P.^ j - 1) b t

-- | Keccak block transformations
-- >>> s = bv2v $(bLit "011") ++ repeat @572 0 ++ singleton 1 ++ repeat 0 :: State 1600
-- >>> hexdump "%02X " s
-- "06 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 "
-- >>> t0 = theta sha3_constants s
-- >>> hexdump "%02X " t0
-- "06 00 00 00 00 00 00 00 06 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 06 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 80 0C 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 06 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 06 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 06 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 80 "
-- >>> t1 = rho sha3_constants t0
-- >>> hexdump "%02X " t1
-- "06 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 40 00 00 00 00 00 00 00 00 00 00 00 64 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 60 00 00 40 00 00 00 00 00 00 00 00 00 00 00 00 00 40 00 00 00 C8 00 00 00 00 00 00 00 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 00 00 00 00 08 00 00 00 00 00 00 00 00 00 00 00 00 00 00 40 06 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 C0 00 00 00 80 00 00 00 00 00 00 00 00 00 00 00 00 00 00 80 0C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 00 00 00 00 00 00 00 20 00 00 00 00 00 00 00 00 00 20 03 00 00 00 00 00 "
-- >>> t2 = pi sha3_constants t1
-- >>> hexdump "%02X " t2
-- "06 00 00 00 00 00 00 00 00 00 00 00 00 60 00 00 00 00 00 00 00 08 00 00 00 00 00 00 00 00 00 00 00 20 03 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 C8 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 C0 00 00 00 00 00 00 00 00 00 20 0C 00 00 00 00 00 00 00 40 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 80 0C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 64 00 00 00 00 00 00 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 40 00 00 00 00 00 00 40 00 00 00 00 00 40 06 00 00 00 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 "
-- >>> t3 = chi sha3_constants t2
-- >>> hexdump "%02X " t3
-- "06 00 00 00 00 08 00 00 00 00 00 00 00 60 00 00 00 20 03 00 00 08 00 00 06 00 00 00 00 00 00 00 00 20 03 00 00 60 00 00 00 00 00 00 00 00 00 00 00 00 C8 00 00 C0 00 00 00 00 00 00 00 00 00 20 00 00 00 00 00 C0 00 00 00 00 C8 00 00 00 00 20 0C 00 00 00 00 00 00 00 C0 0C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 8C 0C 00 00 00 00 00 00 40 00 00 00 00 00 00 00 00 18 00 64 00 00 00 00 00 80 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 80 00 64 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 40 06 00 40 00 00 00 00 00 00 40 00 18 00 00 00 40 06 00 00 00 00 00 00 00 00 00 40 18 00 00 00 00 00 40 00 "
-- >>> t4 = iota sha3_constants 0 t3
-- >>> hexdump "%02X " t4
-- "07 00 00 00 00 08 00 00 00 00 00 00 00 60 00 00 00 20 03 00 00 08 00 00 06 00 00 00 00 00 00 00 00 20 03 00 00 60 00 00 00 00 00 00 00 00 00 00 00 00 C8 00 00 C0 00 00 00 00 00 00 00 00 00 20 00 00 00 00 00 C0 00 00 00 00 C8 00 00 00 00 20 0C 00 00 00 00 00 00 00 C0 0C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 8C 0C 00 00 00 00 00 00 40 00 00 00 00 00 00 00 00 18 00 64 00 00 00 00 00 80 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 80 00 64 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 40 06 00 40 00 00 00 00 00 00 40 00 18 00 00 00 40 06 00 00 00 00 00 00 00 00 00 40 18 00 00 00 00 00 40 00 "
theta :: forall l w b. (KeccakParameter l w b) => SHA3Constants l w b -> State b -> State b
theta c s = fmap (fold xor . fmap (s !!)) $ theta_constants c

rho :: forall l w b. (KeccakParameter l w b) => SHA3Constants l w b -> State b -> State b
rho c s = fmap (s !!) $ rho_constants c

pi :: (KeccakParameter l w b) => SHA3Constants l w b -> State b -> State b
pi c s = fmap (s !!) $ pi_constants c

chi :: forall l w b. (KeccakParameter l w b) => SHA3Constants l w b -> State b -> State b
chi c s = fmap f $ chi_constants c
  where
    f (i0, i1, i2) = s !! i0 `xor` (complement (s !! i1) .&. s !! i2)

iota ::
  forall l w b.
  (KeccakParameter l w b) =>
  SHA3Constants l w b ->
  Index (12 + 2 * l) ->
  State b ->
  State b
iota c i = concat . f . unconcatI @25
  where
    f s = (zipWith xor rc $ head s) :> tail s
    rc = leToPlusKN @w @64 takeI $ iota_constants c !! i
