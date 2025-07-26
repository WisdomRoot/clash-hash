module SHA3
  ( StateArray
  , bs2v
  , v2bs
  , hexdump
  , toBitString
  , theta
  , rho
  , pi
  , chi
  , iota
  , keccakf
  , sponge
  , keccak
  , sha3_224
  , sha3_256
  , sha3_384
  , sha3_512
  ) where

import Data.Bifunctor (Bifunctor(..))
import Data.Modular
import Data.Proxy (Proxy(..))
import qualified Prelude as P
import Text.Printf (printf)

import Clash.Prelude hiding (pi)

-- $

-- $setup
-- >>> import Clash.Prelude hiding (pi)

imapping :: forall s n a. (KnownNat s, KnownNat n, 1 <= n)
         => ((Index s)/n -> Vec n a -> a)
         -> Vec n a
         -> Vec n a
imapping f s = fmap (flip f s . toMod . resize) indicesI

imapping2 :: forall s n m a. (KnownNat s, KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m)
          => (((Index s)/n, (Index s)/m) -> Vec n (Vec m a) -> a)
          -> Vec n (Vec m a)
          -> Vec n (Vec m a)
imapping2 f = unconcatI . imapping g . concat where
  m = fromInteger . natVal $ Proxy @m
  g i = f (bimap toMod toMod $ unMod i `divMod` m) . unconcatI

imapping3 :: forall s n m l a. ( KnownNat s
                               , KnownNat n
                               , KnownNat m
                               , KnownNat l
                               , 1 <= n
                               , 1 <= m
                               , 1 <= l
                               , 1 <= n * m * l)
          => (((Index s)/n, (Index s)/m, (Index s)/l) -> Vec n (Vec m (Vec l a)) -> a)
          -> Vec n (Vec m (Vec l a))
          -> Vec n (Vec m (Vec l a))
imapping3 f = unconcatI . unconcatI . imapping g . concat . concat where
  ml = fromInteger . natVal $ Proxy @(m * l)
  l = fromInteger . natVal $ Proxy @l
  g idx = f (toMod i, toMod j, toMod k) . unconcatI . unconcatI where
    (i, r) = unMod idx `divMod` ml
    (j, k) = r `divMod` l

--

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

type KeccakParameter l w b = ( KnownNat l
                             , KnownNat w
                             , KnownNat b
                             , 3 <= l
                             , l <= 6
                             , 1 <= w
                             , 1 <= b
                             , 1 <= b `Div` 8
                             , w ~ 2 ^ l
                             , b ~ 25 * w
                             , b ~ 5 * (5 * w)
                             , b ~ (b `Div` 8) * 8
                             )

type StateArray w = Vec 5 (Vec 5 (BitString w))
type Index2 b = ((Index (2 * b))/5, (Index (2 * b))/5)
type Index3 w b = ((Index (2 * b))/5, (Index (2 * b))/5, (Index (2 * b))/w)

infixl 9 @@
(@@) :: (KeccakParameter l w b) => StateArray w -> Index2 b -> BitString w
s @@ (i, j) = s !! unMod i !! unMod j

infixl 9 @@@
(@@@) :: (KeccakParameter l w b) => StateArray w -> Index3 w b -> Bit
s @@@ (i, j, k) = s !! unMod i !! unMod j !! unMod k

-- | Keccak block transformations
-- >>> s = unconcatI . unconcatI $ bv2v $(bLit "011") ++ repeat @572 0 ++ singleton 1 ++ repeat 0 :: StateArray 64
-- >>> hexdump "%02X " . concat $ concat s
-- "06 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 "
-- >>> t0 = theta s
-- >>> hexdump "%02X " . concat $ concat t0
-- "06 00 00 00 00 00 00 00 06 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 06 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 80 0C 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 06 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 06 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 06 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 80 "
-- >>> t1 = rho t0
-- >>> hexdump "%02X " . concat $ concat t1
-- "06 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 40 00 00 00 00 00 00 00 00 00 00 00 64 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 60 00 00 40 00 00 00 00 00 00 00 00 00 00 00 00 00 40 00 00 00 C8 00 00 00 00 00 00 00 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 00 00 00 00 08 00 00 00 00 00 00 00 00 00 00 00 00 00 00 40 06 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 C0 00 00 00 80 00 00 00 00 00 00 00 00 00 00 00 00 00 00 80 0C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 00 00 00 00 00 00 00 20 00 00 00 00 00 00 00 00 00 20 03 00 00 00 00 00 "
-- >>> t2 = pi t1
-- >>> hexdump "%02X " . concat $ concat t2
-- "06 00 00 00 00 00 00 00 00 00 00 00 00 60 00 00 00 00 00 00 00 08 00 00 00 00 00 00 00 00 00 00 00 20 03 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 C8 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 C0 00 00 00 00 00 00 00 00 00 20 0C 00 00 00 00 00 00 00 40 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 80 0C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 64 00 00 00 00 00 00 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 40 00 00 00 00 00 00 40 00 00 00 00 00 40 06 00 00 00 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 "
-- >>> t3 = chi t2
-- >>> hexdump "%02X " . concat $ concat t3
-- "06 00 00 00 00 08 00 00 00 00 00 00 00 60 00 00 00 20 03 00 00 08 00 00 06 00 00 00 00 00 00 00 00 20 03 00 00 60 00 00 00 00 00 00 00 00 00 00 00 00 C8 00 00 C0 00 00 00 00 00 00 00 00 00 20 00 00 00 00 00 C0 00 00 00 00 C8 00 00 00 00 20 0C 00 00 00 00 00 00 00 C0 0C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 8C 0C 00 00 00 00 00 00 40 00 00 00 00 00 00 00 00 18 00 64 00 00 00 00 00 80 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 80 00 64 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 40 06 00 40 00 00 00 00 00 00 40 00 18 00 00 00 40 06 00 00 00 00 00 00 00 00 00 40 18 00 00 00 00 00 40 00 "
-- >>> t4 = iota 0 t3
-- >>> hexdump "%02X " . concat $ concat t4
-- "07 00 00 00 00 08 00 00 00 00 00 00 00 60 00 00 00 20 03 00 00 08 00 00 06 00 00 00 00 00 00 00 00 20 03 00 00 60 00 00 00 00 00 00 00 00 00 00 00 00 C8 00 00 C0 00 00 00 00 00 00 00 00 00 20 00 00 00 00 00 C0 00 00 00 00 C8 00 00 00 00 20 0C 00 00 00 00 00 00 00 C0 0C 00 00 00 00 00 00 00 00 00 00 00 00 00 00 8C 0C 00 00 00 00 00 00 40 00 00 00 00 00 00 00 00 18 00 64 00 00 00 00 00 80 00 00 00 00 00 00 00 18 00 00 00 00 00 00 00 80 00 64 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 40 06 00 40 00 00 00 00 00 00 40 00 18 00 00 00 40 06 00 00 00 00 00 00 00 00 00 40 18 00 00 00 00 00 40 00 "

theta :: (KeccakParameter l w b) => StateArray w -> StateArray w
theta = imapping3 $ \(i, j, k) s -> s @@@ (i, j, k)
  `xor` s @@@ (0, j - 1, k)     `xor` s @@@ (1, j - 1, k)     `xor` s @@@ (2, j - 1, k)     `xor` s @@@ (3, j - 1, k)     `xor` s @@@ (4, j - 1, k)
  `xor` s @@@ (0, j + 1, k - 1) `xor` s @@@ (1, j + 1, k - 1) `xor` s @@@ (2, j + 1, k - 1) `xor` s @@@ (3, j + 1, k - 1) `xor` s @@@ (4, j + 1, k - 1)

rho :: forall l w b. (KeccakParameter l w b) => StateArray w -> StateArray w
rho = imapping2 g where
  compareSwap a b = if fst a > fst b then (a,b) else (b,a)
  insert y xs = let (y',xs') = mapAccumL compareSwap y xs in xs' :< y'
  sort = vfold $ const insert
  z = (0 :: Integer, 0 :: Integer/5, 1, 1)
  f (t, i, j, k) = ((5*(unMod i) + unMod j, k), (t + 1, 3*i + 2*j, i, k * (t + 3) `div` (t + 1)))
  r = unconcatI $ 0 :> (fmap fromInteger . snd . unzip . sort $ unfoldrI f z) :: Vec 5 (Vec 5 ((Index b)/w))
  g (i, j) s = rotateRight (s @@ (i, j)) . unMod $ r !! unMod i !! unMod j

pi :: (KeccakParameter l w b) => StateArray w -> StateArray w
pi = imapping2 $ \(i, j) s -> s @@ (j, 3*i + j)

chi :: (KeccakParameter l w b) => StateArray w -> StateArray w
chi = imapping3 $ \(i, j, k) s -> s @@@ (i, j, k) `xor` (complement (s @@@ (i, j + 1, k)) .&. s @@@ (i, j + 2, k))

iota :: forall l w b. (KeccakParameter l w b) => Index (12 + 2 * l) -> StateArray w -> StateArray w
iota i s = unconcatI . replace (0 :: Index 25) (zipWith xor (rcs !! i) $ s @@ (0, 0)) $ concat s where
  lfsr = unconcatI . unfoldrI f $ bv2v $(bLit "10000000") :: Vec (12 + 2 * l) (BitString 7)
  f t = (head t, zipWith xor (0 +>> t) . fmap (last t .&.) $ bv2v $(bLit "10001110"))
  g t j b = replace @_ @(Unsigned (l + 1)) (2 P.^ j - 1) b t
  rcs = fmap (ifoldl g $ repeat 0) lfsr

-- | Keccak-f[1600] & Keccak[1024]
-- >>> s = unconcatI . unconcatI $ bv2v $(bLit "011") ++ repeat @572 0 ++ singleton 1 ++ repeat 0 :: StateArray 64
-- >>> hexdump "%02X " . concat . concat $ keccakf s
-- "A6 9F 73 CC A2 3A 9A C5 C8 B5 67 DC 18 5A 75 6E 97 C9 82 16 4F E2 58 59 E0 D1 DC C1 47 5C 80 A6 15 B2 12 3A F1 F5 F9 4C 11 E3 E9 40 2C 3A C5 58 F5 00 19 9D 95 B6 D3 E3 01 75 85 86 28 1D CD 26 36 4B C5 B8 E7 8F 53 B8 23 DD A7 F4 DE 9F AD 00 E6 7D B7 2F 9F 9F EA 0C E3 C9 FE F1 5A 76 AD C5 85 EB 2E FD 11 87 FB 65 F9 C9 A2 73 31 51 67 E3 14 FA 68 B6 A3 22 D4 07 01 5D 50 2A CD EC 8C 88 5C 4F 77 84 CE D0 46 09 BB 35 15 4A 96 48 4B 56 25 D3 41 7C 88 60 7A CD E4 C2 C9 9B AE 5E DF 9E EA 2A D0 FB 55 A2 26 18 9E 11 D2 49 60 43 3E 2B 0E E0 45 A4 73 09 97 76 DD 5D E7 39 DB 9B A8 19 D5 4C B9 03 A7 A5 D7 EE "
-- >>> i = bv2v $(bLit "01")
-- >>> hexdump "%02X " $ keccak @1024 @512 i
-- "A6 9F 73 CC A2 3A 9A C5 C8 B5 67 DC 18 5A 75 6E 97 C9 82 16 4F E2 58 59 E0 D1 DC C1 47 5C 80 A6 15 B2 12 3A F1 F5 F9 4C 11 E3 E9 40 2C 3A C5 58 F5 00 19 9D 95 B6 D3 E3 01 75 85 86 28 1D CD 26 "

keccakf :: (KeccakParameter l w b) => StateArray w -> StateArray w
keccakf = foldl (flip f) id indicesI where f i = (.) (iota i . chi . pi . rho . theta)

type SpongeParameter b r n m d = ( KnownNat b
                                 , KnownNat r
                                 , KnownNat n
                                 , KnownNat m
                                 , KnownNat d
                                 , 1 <= b
                                 , 1 <= r
                                 , r <= b - 1
                                 , 1 <= n
                                 , n ~ (m + r + 1) `Div` r
                                 , m + 2 <= n * r
                                 , n * r <= m + r + 1
                                 , d <= r
                                 , d <= b
                                 )

sponge :: forall b r n m d. (SpongeParameter b r n m d)
       => (BitString b -> BitString b)
       -> BitString m
       -> BitString d
sponge f i = leToPlusKN @d @b takeI $ foldl g (repeat 0) v where
  g s = f . zipWith xor s . flip (++) (0 :> repeat @(b - 1 - r) 0)
  v = unconcatI @n @r $ i ++ singleton 1 ++ repeat @(n * r - (m + 2)) 0 ++ singleton 1

keccak :: forall c d r n m. ( KnownNat c
                            , 1 <= c
                            , c <= 1599
                            , r ~ 1600 - c
                            , SpongeParameter 1600 r n m d
                            )
       => BitString m
       -> BitString d
keccak = sponge @1600 @r @n @m @d $ concat . concat . keccakf @6 @64 @1600 . unconcatI . unconcatI

-- | SHA3
-- >>> hexdump "%02x" . sha3_224 . v2bs $ toBitString $(listToVecTH "abc")
-- "e642824c3f8cf24ad09234ee7d3c766fc9a3a5168d0c94ad73b46fdf"
-- >>> hexdump "%02x" . sha3_256 . v2bs $ toBitString $(listToVecTH "abc")
-- "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"
-- >>> hexdump "%02x" . sha3_384 . v2bs $ toBitString $(listToVecTH "abc")
-- "ec01498288516fc926459f58e2c6ad8df9b473cb0fc08c2596da7cf0e49be4b298d88cea927ac7f539f1edf228376d25"
-- >>> hexdump "%02x" . sha3_512 . v2bs $ toBitString $(listToVecTH "abc")
-- "b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0"
-- >>> hexdump "%02x" . sha3_224 . v2bs $ toBitString $(listToVecTH "")
-- "6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7"
-- >>> hexdump "%02x" . sha3_256 . v2bs $ toBitString $(listToVecTH "")
-- "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"
-- >>> hexdump "%02x" . sha3_384 . v2bs $ toBitString $(listToVecTH "")
-- "0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004"
-- >>> hexdump "%02x" . sha3_512 . v2bs $ toBitString $(listToVecTH "")
-- "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26"

sha3_224 :: forall m n. (KnownNat m, SpongeParameter 1600 1152 n (m + 2) 224)
         => BitString m -> BitString 224
sha3_224 = keccak @448 @224 @1152 @n @(m + 2) . flip (++) (bv2v $(bLit "01"))

sha3_256 :: forall m n. (KnownNat m, SpongeParameter 1600 1088 n (m + 2) 256)
         => BitString m -> BitString 256
sha3_256 = keccak @512 @256 @1088 @n @(m + 2) . flip (++) (bv2v $(bLit "01"))

sha3_384 :: forall m n. (KnownNat m, SpongeParameter 1600 832 n (m + 2) 384)
         => BitString m -> BitString 384
sha3_384 = keccak @768 @384 @832 @n @(m + 2) . flip (++) (bv2v $(bLit "01"))

sha3_512 :: forall m n. (KnownNat m, SpongeParameter 1600 576 n (m + 2) 512)
         => BitString m -> BitString 512
sha3_512 = keccak @1024 @512 @576 @n @(m + 2) . flip (++) (bv2v $(bLit "01"))
