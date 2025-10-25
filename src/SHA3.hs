{-# LANGUAGE TemplateHaskell #-}

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
  , shake_128
  , shake_256
  , topEntity
  ) where

import qualified Prelude as P
import Clash.Prelude hiding (pi)

import Data.Modular
import Data.Proxy (Proxy(..))
import Text.Printf (printf)

import SHA3constants

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
                             , w <= 64
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
theta = imapping3 $ \ (i, j, k) s -> s @@@ (i, j, k)
  `xor` s @@@ (0, j - 1, k)     `xor` s @@@ (1, j - 1, k)     `xor` s @@@ (2, j - 1, k)     `xor` s @@@ (3, j - 1, k)     `xor` s @@@ (4, j - 1, k)
  `xor` s @@@ (0, j + 1, k - 1) `xor` s @@@ (1, j + 1, k - 1) `xor` s @@@ (2, j + 1, k - 1) `xor` s @@@ (3, j + 1, k - 1) `xor` s @@@ (4, j + 1, k - 1)

rho :: forall l w b. (KeccakParameter l w b) => StateArray w -> StateArray w
rho = imapping2 $ \ (i, j) s -> rotateRight (s @@ (i, j)) $ r !! unMod i !! unMod j where
  r = $(lift rho_constant) :: Vec 5 (Vec 5 Int)

pi :: (KeccakParameter l w b) => StateArray w -> StateArray w
pi = imapping2 $ \ (i, j) s -> s @@ (j, 3*i + j)

chi :: (KeccakParameter l w b) => StateArray w -> StateArray w
chi = imapping3 $ \ (i, j, k) s -> s @@@ (i, j, k) `xor` (complement (s @@@ (i, j + 1, k)) .&. s @@@ (i, j + 2, k))

iota :: forall l w b. (KeccakParameter l w b) => Index (12 + 2 * l) -> StateArray w -> StateArray w
iota i s = unconcatI . replace (0 :: Index 25) (zipWith xor rc $ s @@ (0, 0)) $ concat s where
  rc = leToPlusKN @w @64 takeI $ $(lift iota_constant) !! i

-- | Keccak-f[1600] & Keccak[1024]
-- >>> s = unconcatI . unconcatI $ bv2v $(bLit "011") ++ repeat @572 0 ++ singleton 1 ++ repeat 0 :: StateArray 64
-- >>> hexdump "%02X " . concat . concat $ keccakf s
-- "A6 9F 73 CC A2 3A 9A C5 C8 B5 67 DC 18 5A 75 6E 97 C9 82 16 4F E2 58 59 E0 D1 DC C1 47 5C 80 A6 15 B2 12 3A F1 F5 F9 4C 11 E3 E9 40 2C 3A C5 58 F5 00 19 9D 95 B6 D3 E3 01 75 85 86 28 1D CD 26 36 4B C5 B8 E7 8F 53 B8 23 DD A7 F4 DE 9F AD 00 E6 7D B7 2F 9F 9F EA 0C E3 C9 FE F1 5A 76 AD C5 85 EB 2E FD 11 87 FB 65 F9 C9 A2 73 31 51 67 E3 14 FA 68 B6 A3 22 D4 07 01 5D 50 2A CD EC 8C 88 5C 4F 77 84 CE D0 46 09 BB 35 15 4A 96 48 4B 56 25 D3 41 7C 88 60 7A CD E4 C2 C9 9B AE 5E DF 9E EA 2A D0 FB 55 A2 26 18 9E 11 D2 49 60 43 3E 2B 0E E0 45 A4 73 09 97 76 DD 5D E7 39 DB 9B A8 19 D5 4C B9 03 A7 A5 D7 EE "
-- >>> i = bv2v $(bLit "01")
-- >>> hexdump "%02X " $ keccak @1024 @512 i
-- "A6 9F 73 CC A2 3A 9A C5 C8 B5 67 DC 18 5A 75 6E 97 C9 82 16 4F E2 58 59 E0 D1 DC C1 47 5C 80 A6 15 B2 12 3A F1 F5 F9 4C 11 E3 E9 40 2C 3A C5 58 F5 00 19 9D 95 B6 D3 E3 01 75 85 86 28 1D CD 26 "

keccakp :: forall b nr l w. ( KeccakParameter l w b
                            , KnownNat nr
                            , 1 <= nr
                            , nr <= 12 + 2 * l
                            )
        => StateArray w
        -> StateArray w
keccakp = foldl (flip f) id $ dropI @(12 + 2 * l - nr) indicesI where f i = (.) (iota i . chi . pi . rho . theta)

keccakf :: forall l w b. (KeccakParameter l w b) => StateArray w -> StateArray w
keccakf = keccakp @b @(12 + 2 * l) @l @w

type SpongeParameter b r n m k d = ( KnownNat b
                                   , KnownNat r
                                   , KnownNat n
                                   , KnownNat m
                                   , KnownNat k
                                   , KnownNat d
                                   , 1 <= b
                                   , 1 <= r
                                   , r <= b - 1
                                   , r <= b
                                   , 1 <= n
                                   , n ~ (m + r + 1) `Div` r
                                   , m + 2 <= n * r
                                   , n * r <= m + r + 1
                                   , k ~ d `Div` r
                                   , d <= (k + 1) * r
                                   )

sponge :: forall b r n m k d. (SpongeParameter b r n m k d)
       => (BitString b -> BitString b)
       -> BitString m
       -> BitString d
sponge f = trunc . squeeze . absorb . pad where
  pad :: BitString m -> Vec n (BitString r)
  pad x = unconcatI $ x ++ singleton 1 ++ repeat @(n * r - (m + 2)) 0 ++ singleton 1
  absorb :: Vec n (BitString r) -> BitString b
  absorb = foldl g $ repeat 0 where g s = f . zipWith xor s . flip (++) (repeat @(b - r) 0)
  squeeze :: BitString b -> Vec (k + 1) (BitString r)
  squeeze = map (leToPlusKN @r @b takeI) . iterateI f
  trunc :: Vec (k + 1) (BitString r) -> BitString d
  trunc = leToPlusKN @d @((k + 1) * r) takeI . concat

keccak :: forall c d r n m k. ( KnownNat c
                              , 1 <= c
                              , c <= 1599
                              , r ~ 1600 - c
                              , SpongeParameter 1600 r n m k d
                              )
       => BitString m
       -> BitString d
keccak = sponge @1600 @r @n @m @k @d $ concat . concat . keccakf @6 @64 @1600 . unconcatI . unconcatI

-- | SHA3 hash functions
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
-- >>> hexdump "%02x" . sha3_224 . v2bs @_ @(Unsigned 8) . unpack $ pack (0x31c82d71785b7ca6b651cb6c8c9ad5e2aceb0b0633c088d33aa247ada7a594ff4936c023251319820a9b19fc6c48de8a6f7ada214176ccdaadaeef51ed43714ac0c8269bbd497e46e78bb5e58196494b2471b1680e2d4c6dbd249831bd83a4d3be06c8a2e903933974aa05ee748bfe6ef359f7a143edf0d4918da916bd6f15e26a790cff514b40a5da7f72e1ed2fe63a05b8149587bea05653718cc8980eadbfeca85b7c9c286dd040936585938be7f98219700c83a9443c2856a80ff46852b26d1b1edf72a30203cf6c44a10fa6eaf1920173cedfb5c4cf3ac665b37a86ed02155bbbf17dc2e786af9478fe0889d86c5bfa85a242eb0854b1482b7bd16f67f80bef9c7a628f05a107936a64273a97b0088b0e515451f916b5656230a12ba6dc78 :: Unsigned 2312)
-- "aab23c9e7fb9d7dacefdfd0b1ae85ab1374abff7c4e3f7556ecae412"
-- >>> hexdump "%02x" . sha3_256 . v2bs @_ @(Unsigned 8) . unpack $ pack (0xb1caa396771a09a1db9bc20543e988e359d47c2a616417bbca1b62cb02796a888fc6eeff5c0b5c3d5062fcb4256f6ae1782f492c1cf03610b4a1fb7b814c057878e1190b9835425c7a4a0e182ad1f91535ed2a35033a5d8c670e21c575ff43c194a58a82d4a1a44881dd61f9f8161fc6b998860cbe4975780be93b6f87980bad0a99aa2cb7556b478ca35d1f3746c33e2bb7c47af426641cc7bbb3425e2144820345e1d0ea5b7da2c3236a52906acdc3b4d34e474dd714c0c40bf006a3a1d889a632983814bbc4a14fe5f159aa89249e7c738b3b73666bac2a615a83fd21ae0a1ce7352ade7b278b587158fd2fabb217aa1fe31d0bda53272045598015a8ae4d8cec226fefa58daa05500906c4d85e7567 :: Unsigned 2184)
-- "cb5648a1d61c6c5bdacd96f81c9591debc3950dcf658145b8d996570ba881a05"
-- >>> hexdump "%02x" . sha3_384 . v2bs @_ @(Unsigned 8) . unpack $ pack (0x5fe35923b4e0af7dd24971812a58425519850a506dfa9b0d254795be785786c319a2567cbaa5e35bcf8fe83d943e23fa5169b73adc1fcf8b607084b15e6a013df147e46256e4e803ab75c110f77848136be7d806e8b2f868c16c3a90c14463407038cb7d9285079ef162c6a45cedf9c9f066375c969b5fcbcda37f02aacff4f31cded3767570885426bebd9eca877e44674e9ae2f0c24cdd0e7e1aaf1ff2fe7f80a1c4f5078eb34cd4f06fa94a2d1eab5806ca43fd0f06c60b63d5402b95c70c21ea65a151c5cfaf8262a46be3c722264b :: Unsigned 1672)
-- "3054d249f916a6039b2a9c3ebec1418791a0608a170e6d36486035e5f92635eaba98072a85373cb54e2ae3f982ce132b"
-- >>> hexdump "%02x" . sha3_512 . v2bs @_ @(Unsigned 8) . unpack $ pack (0x664ef2e3a7059daf1c58caf52008c5227e85cdcb83b4c59457f02c508d4f4f69f826bd82c0cffc5cb6a97af6e561c6f96970005285e58f21ef6511d26e709889a7e513c434c90a3cf7448f0caeec7114c747b2a0758a3b4503a7cf0c69873ed31d94dbef2b7b2f168830ef7da3322c3d3e10cafb7c2c33c83bbf4c46a31da90cff3bfd4ccc6ed4b310758491eeba603a76 :: Unsigned 1160)
-- "e5825ff1a3c070d5a52fbbe711854a440554295ffb7a7969a17908d10163bfbe8f1d52a676e8a0137b56a11cdf0ffbb456bc899fc727d14bd8882232549d914e"

sha3_224 :: forall m n. (KnownNat m, SpongeParameter 1600 1152 n (m + 2) 0 224)
         => BitString m -> BitString 224
sha3_224 = keccak @448 @224 @1152 @n @(m + 2) @0 . flip (++) (bv2v $(bLit "01"))

sha3_256 :: forall m n. (KnownNat m, SpongeParameter 1600 1088 n (m + 2) 0 256)
         => BitString m -> BitString 256
sha3_256 = keccak @512 @256 @1088 @n @(m + 2) @0 . flip (++) (bv2v $(bLit "01"))

sha3_384 :: forall m n. (KnownNat m, SpongeParameter 1600 832 n (m + 2) 0 384)
         => BitString m -> BitString 384
sha3_384 = keccak @768 @384 @832 @n @(m + 2) @0 . flip (++) (bv2v $(bLit "01"))

sha3_512 :: forall m n. (KnownNat m, SpongeParameter 1600 576 n (m + 2) 0 512)
         => BitString m -> BitString 512
sha3_512 = keccak @1024 @512 @576 @n @(m + 2) @0 . flip (++) (bv2v $(bLit "01"))

-- | SHA3 extended output functions
-- >>> hexdump "%02x" . shake_128 @_ @1120 . v2bs @_ @(Unsigned 8) . unpack $ pack (0x0a13ad2c7a239b4ba73ea6592ae84ea9 :: Unsigned 128)
-- "5feaf99c15f48851943ff9baa6e5055d8377f0dd347aa4dbece51ad3a6d9ce0c01aee9fe2260b80a4673a909b532adcdd1e421c32d6460535b5fe392a58d2634979a5a104d6c470aa3306c400b061db91c463b2848297bca2bc26d1864ba49d7ff949ebca50fbf79a5e63716dc82b600bd52ca7437ed774d169f6bf02e46487956fba2230f34cd2a0485484d"
-- >>> hexdump "%02x" . shake_256 @_ @2000 . v2bs @_ @(Unsigned 8) . unpack $ pack (0x8d8001e2c096f1b88e7c9224a086efd4797fbf74a8033a2d422a2b6b8f6747e4 :: Unsigned 256)
-- "2e975f6a8a14f0704d51b13667d8195c219f71e6345696c49fa4b9d08e9225d3d39393425152c97e71dd24601c11abcfa0f12f53c680bd3ae757b8134a9c10d429615869217fdd5885c4db174985703a6d6de94a667eac3023443a8337ae1bc601b76d7d38ec3c34463105f0d3949d78e562a039e4469548b609395de5a4fd43c46ca9fd6ee29ada5efc07d84d553249450dab4a49c483ded250c9338f85cd937ae66bb436f3b4026e859fda1ca571432f3bfc09e7c03ca4d183b741111ca0483d0edabc03feb23b17ee48e844ba2408d9dcfd0139d2e8c7310125aee801c61ab7900d1efc47c078281766f361c5e6111346235e1dc38325666c"

shake_128 :: forall m d n k. (KnownNat m, SpongeParameter 1600 1344 n (m + 4) k d)
         => BitString m -> BitString d
shake_128 = keccak @256 @d @1344 @n @(m + 4) @k . flip (++) (bv2v $(bLit "1111"))

shake_256 :: forall m d n k. (KnownNat m, SpongeParameter 1600 1088 n (m + 4) k d)
         => BitString m -> BitString d
shake_256 = keccak @512 @d @1088 @n @(m + 4) @k . flip (++) (bv2v $(bLit "1111"))

--

{-# ANN topEntity
  (Synthesize
    { t_name = "SHA3_256"
    , t_inputs = [ PortName "CLK"
                 , PortName "RST"
                 , PortName "EN"
                 , PortName "DIN"
                 ]
    , t_output = PortName "DOUT"
    }) #-}
{-# OPAQUE topEntity #-}

topEntity :: Clock System
          -> Reset System
          -> Enable System
          -> Signal System (StateArray 64)
          -> Signal System (StateArray 64)
topEntity = exposeClockResetEnable $ fmap (iota 0 . chi . pi . rho . theta)
