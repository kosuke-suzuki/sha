import Data.Char (ord)
import Data.Word (Word8)
import Test.Hspec
import Lib

stringToWord8List :: String -> [Word8]
stringToWord8List = map (fromIntegral . ord)

main :: IO ()
main = hspec $ do
    describe "Lib.zeroes" $ do
        it "returns a list of zeroes of the specified number" $ do
            zeroes 0 `shouldBe` []
            zeroes 1 `shouldBe` [0]
            zeroes 10 `shouldBe` [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

    describe "Lib.padSize" $ do
        it "returns the number of zeroes to pad" $ do
            (padSize []) `shouldBe` 55
            (padSize [0]) `shouldBe` 54
            (padSize (take 8 $ repeat 1)) `shouldBe` 47
        
        it "returns 0 when the input's length is 55 mod 64" $ do
            (padSize (take 55 $ repeat 1)) `shouldBe` 0
            (padSize (take 119 $ repeat 1)) `shouldBe` 0

        it "returns 64 when the input's length is 56 mod 64" $ do
            (padSize (take 56 $ repeat 1)) `shouldBe` 63
            (padSize (take 120 $ repeat 1)) `shouldBe` 63

    describe "Lib.pad" $ do
        it "returns a list of zeroes to pad" $ do
            (pad []) `shouldBe` (take 55 $ repeat 0)
            (pad [0]) `shouldBe` (take 54 $ repeat 0)
            (pad (take 8 $ repeat 1)) `shouldBe` (take 47 $ repeat 0)

    describe "Lib.lengthIndicator" $ do
        it "returns the length of the input in 64 bits" $ do
            (lengthIndicator []) `shouldBe` [0, 0, 0, 0, 0, 0, 0, 0]
            (lengthIndicator [0]) `shouldBe` [0, 0, 0, 0, 0, 0, 0, 8]
            (lengthIndicator (take 8 $ repeat 1)) `shouldBe` [0, 0, 0, 0, 0, 0, 0, 64]

    describe "Lib.prepare" $ do
        it "returns the input with padding and length indicator" $ do
            (prepare []) `shouldBe` 0x80:(zeroes 63)

        it "returns the list whose length is 64 if input's length is less than or equal to 55" $ do
            length (prepare [1]) `shouldBe` 64
            length (prepare (take 55 $ repeat 1)) `shouldBe` 64

        it "returns the list whose length is 128 if input's length is greater than 55 and less than or equal to 119" $ do
            length (prepare (take 56 $ repeat 1)) `shouldBe` 128
            length (prepare (take 119 $ repeat 1)) `shouldBe` 128

    describe "Lib.convert8to32" $ do
        it "converts 4 Word8s to 1 Word32 in big endians" $ do
            (convert8to32 [0, 0, 0, 0]) `shouldBe` 0
            (convert8to32 [0, 0, 0, 1]) `shouldBe` 1
            (convert8to32 [0, 0, 1, 0]) `shouldBe` 256
            (convert8to32 [0, 1, 0, 0]) `shouldBe` 65536
            (convert8to32 [1, 0, 0, 0]) `shouldBe` 16777216
    
    describe "Lib.chunksOf" $ do
        it "splits a list into chunks of the specified size" $ do
            (chunksOf 4 [1, 2, 3, 4, 5, 6, 7, 8]) `shouldBe` [[1, 2, 3, 4], [5, 6, 7, 8]]

    describe "Lib.f" $ do
        it "returns the correct value" $ do
            (f 0 0 0 0) `shouldBe` 0
            (f 0 1 1 1) `shouldBe` 1
            (f 20 1 1 1) `shouldBe` 1
            (f 40 1 1 1) `shouldBe` 1
            (f 60 1 1 1) `shouldBe` 1

    describe "Lib.rotateLeft" $ do
        it "rotates the bits to the left" $ do
            (rotateLeft 1 1) `shouldBe` 2
        
        it "rotates the highest bit to the lowest bit" $ do
            (rotateLeft 1 0x80000000) `shouldBe` 1

    describe "Lib.wBuild" $ do
        it "returns w which returns the element of the original list in [0, 15]" $ do
            (wBuild (take 16 $ repeat 1) 0) `shouldBe` 1

        it "retruns w which returns the calculated element from the original list in [16, 79]" $ do
                        (wBuild (take 16 $ repeat 1) 16) `shouldBe` 0
                        (wBuild (take 16 $ repeat 1) 79) `shouldBe` 3631408
    
    describe "Lib.iterateT" $ do
        it "iterates once over t" $ do
            (iterateT (wBuild (take 16 $ repeat 1)) (0, 0, 0, 0, 0) 0) `shouldBe` (1518500250, 0, 0, 0, 0)

    describe "Lib.iterateN" $ do
        it "iterates once over n" $ do
            (iterateN (0, 0, 0, 0, 0) (take 64 $ repeat 1)) `shouldBe` (1816315895, 2967276398, 2688697021, 3133102683, 2767092828)

    describe "Lib.vector2String" $ do
        it "shows a vector as a string" $ do
            (vector2String (0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff)) `shouldBe` "ffffffffffffffffffffffffffffffffffffffff"
        
        it "shows always 40 characters even when the head of input is 0" $ do
            (vector2String (0, 0, 0, 0, 0)) `shouldBe` "0000000000000000000000000000000000000000"

    describe "Lib.calcSha1" $ do
        it "returns SHA-1 hash" $ do
            (calcSha1 $ stringToWord8List "abc") `shouldBe` "a9993e364706816aba3e25717850c26c9cd0d89d"
            (calcSha1 $ stringToWord8List "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq") `shouldBe` "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
            (calcSha1 $ stringToWord8List "The quick brown fox jumps over the lazy dog") `shouldBe` "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12"
            (calcSha1 $ stringToWord8List "The quick brown fox jumps over the lazy cog") `shouldBe` "de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3"
            (calcSha1 $ stringToWord8List "a") `shouldBe` "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"
            (calcSha1 $ stringToWord8List "0123456701234567012345670123456701234567012345670123456701234567") `shouldBe` "e0c094e867ef46c350ef54a7f59dd60bed92ae83"
