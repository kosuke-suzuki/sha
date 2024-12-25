import Data.Array
import Data.Bits
import Data.Word
import Numeric

zeroes :: Int -> [Word8]
zeroes n = take n $ repeat 0

unitSize = 64 -- 512 bits
lengthIndicatorSize = 8 -- 64 bits

padSize :: [a] -> Int
padSize ws = unitSize - 1 - mod (length ws) unitSize - lengthIndicatorSize

pad :: [Word8] -> [Word8]
pad ws = zeroes $ padSize ws

lengthIndicator :: [Word8] -> [Word8]
lengthIndicator ws = numInBits $ length ws

-- n < 2^64 == 256^8
numInBits :: Int -> [Word8]
numInBits n = map (fromIntegral . (shiftR (8 * n)) . ((*) 8)) [7, 6..0]

prepare :: [Word8] -> [Word8]
prepare ws = ws ++ (0x80:(pad ws)) ++ (lengthIndicator ws)

-- 4 Word8s to 1 Word32 in big endians
convert8to32 :: [Word8] -> Word32
convert8to32 [w1, w2, w3, w4] =
  (fromIntegral w1 `shiftL` 24) .|.
  (fromIntegral w2 `shiftL` 16) .|.
  (fromIntegral w3 `shiftL`  8) .|.
  fromIntegral w4

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

k :: Int -> Word32
k t
  |  0 <= t && t <= 19 = 0x5a827999
  | 20 <= t && t <= 39 = 0x6ed9eba1
  | 40 <= t && t <= 59 = 0x8f1bbcdc
  | 60 <= t && t <= 79 = 0xca62c1d6

f :: Int -> Word32 -> Word32 -> Word32 -> Word32
f t b c d
  |  0 <= t && t <= 19 = (b .&. c) .|. (complement b .&. d)
  | 20 <= t && t <= 39 = b `xor` c `xor` d
  | 40 <= t && t <= 59 = (b .&. c) .|. (b .&. d) .|. (c .&. d)
  | 60 <= t && t <= 79 = b `xor` c `xor` d

type Vector = (Word32, Word32, Word32, Word32, Word32)

iv :: Vector
iv = (
  0x67452301,
  0xefcdab89,
  0x98badcfe,
  0x10325476,
  0xc3d2e1f0)

rotateLeft :: Int -> Word32 -> Word32
rotateLeft n w = (shiftL w n) .|. (shiftR w (32 - n))
s1  = rotateLeft 1
s5  = rotateLeft 5
s30 = rotateLeft 30

-- 0 <= t <= 79
wBuild :: [Word32] -> Int -> Word32
wBuild ws t = memoArray ! t
  where
    memoArray = listArray (0, 79) (
        ws ++ 
          [
            s1 $ (memoArray ! (t -  3)) `xor`
                 (memoArray ! (t -  8)) `xor`
                 (memoArray ! (t - 14)) `xor`
                 (memoArray ! (t - 16))
            | t <- [16..79]
          ]
      )

iterateT :: (Int -> Word32) -> Vector -> Int -> Vector
iterateT w (a, b, c, d, e) t = ((s5 a) + (f t b c d) + e + (w t) + (k t), a, s30 b, c, d)

iterateN :: Vector -> [Word32] -> Vector
iterateN (a0, b0, c0, d0, e0) ws = (a0 + a1, b0 + b1, c0 + c1, d0 + d1, e0 + e1)
  where
    (a1, b1, c1, d1, e1) = foldl (iterateT (wBuild ws)) (a0, b0, c0, d0, e0) [0..79]

calcSha1 :: [Word8] -> Vector
calcSha1 = (foldl iterateN iv) . (chunksOf 16) . (Prelude.map convert8to32) . (chunksOf 4) . prepare

accumulateHex :: String -> Word32 -> String
accumulateHex xs y = showHex y xs

vector2String :: Vector -> String
vector2String (a, b, c, d, e) = foldl accumulateHex "" [e, d, c, b, a]
