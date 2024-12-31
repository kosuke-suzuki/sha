module Lib where

import Control.Monad (replicateM)
import Data.Array
import Data.Binary.Get (runGet, getWord32be)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word
import System.Environment
import System.IO
import Text.Printf

zeroes :: Int -> [Word8]
zeroes n = take n $ repeat 0

unitSize = 64 -- 512 bits
lengthIndicatorSize = 8 -- 64 bits

padSize :: Int -> Int
padSize n = mod (unitSize - n - lengthIndicatorSize - 1) unitSize

pad :: Int -> [Word8]
pad = zeroes . padSize

lengthIndicator :: Int -> [Word8]
lengthIndicator = numInBits lengthIndicatorSize

-- n < 2^64 == 256^8
numInBits :: Int -> Int -> [Word8]
numInBits len n = reverse $ map (fromIntegral . (shiftR (8 * n)) . ((*) 8)) $ take len [0..]

footer :: Int -> BS.ByteString
footer n = BS.pack $ 0x80:(pad n) ++ lengthIndicator n

prepare :: BS.ByteString -> BS.ByteString
prepare ws = ws `BS.append` (footer $ BS.length ws)

-- 4 elements of ByteString to 1 Word32 in big endians
convertChunkToWord32 :: BS.ByteString -> [Word32]
convertChunkToWord32 chunk = runGet (replicateM 16 getWord32be) (BS.fromStrict chunk)

foldlChunksOf :: Int -> (a -> BS.ByteString -> a) -> a -> BS.ByteString -> a
foldlChunksOf n f acc ws
  | BS.null ws = acc
  | otherwise  = foldlChunksOf n f (f acc (BS.take n ws)) (BS.drop n ws)

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

iterateN :: Vector -> BS.ByteString -> Vector
iterateN (a0, b0, c0, d0, e0) ws = (a0 + a1, b0 + b1, c0 + c1, d0 + d1, e0 + e1)
  where
    (a1, b1, c1, d1, e1) = foldl (iterateT (wBuild $ convertChunkToWord32 ws)) (a0, b0, c0, d0, e0) [0..79]

vector2String :: Vector -> String
vector2String (a, b, c, d, e) = concatMap (printf "%08x") [a, b, c, d, e]

calcSha1 :: BS.ByteString -> String
calcSha1 = vector2String . foldlChunksOf 64 iterateN iv . prepare

getFileSha1 :: IO ()
getFileSha1 = do
  args <- getArgs
  if null args
    then putStrLn "Specify a file path."
  else
    do
      let filePath = head args
      content <- BS.readFile filePath
      putStrLn $ calcSha1 content
