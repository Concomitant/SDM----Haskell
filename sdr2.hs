{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Word
import Data.Bits as Bits
import Data.List.Split

range :: Int
range = 4

x :: Data.Word.Word8
x = 1

listSum :: (Num a) => [a] -> [a] -> [a]
listSum = zipWith (+)

bitsToCounters :: (Num t, Bits a) => a -> [t]
bitsToCounters thebits = [counter thebits x| x <- reverse [0..(Bits.bitSize thebits - 1)]] 

wordsToCounters :: (Num b, Bits a) => [a] -> [b]
wordsToCounters x = concatMap bitsToCounters x

sumWords :: [[Int]] -> [Int]
sumWords wordlist = foldl1  listSum wordlist

mapByte :: (Bits.Bits a) => (Bool -> b) -> a -> [b] 
mapByte = undefined

counter :: (Num a1, Bits a) => a -> Int -> a1
counter a b
	| Bits.testBit a b = 1
	| otherwise        = -1

result x = map B.unpack (M.elems x)

filterKey predicate source = M.filterWithKey (\k a -> predicate k) source

hamming ::  Bits a => a -> a -> Int
hamming a b = Bits.popCount (Bits.xor a b)

hamming' :: Bits b => [b] -> [b] -> Int
hamming' a b = sum $ zipWith hamming a b

hamming'' a b = hamming' (B.unpack a) (B.unpack b)

bits :: [Data.Word.Word8] -> B.ByteString
bits = B.pack

hardAddresses :: M.Map C.ByteString [Int]
hardAddresses = M.fromList [(bits [0,0,0,0], wordsToCounters ([0,0,0,1]::[Word8])),
							(bits [1,1,1,1], wordsToCounters ([1,3,1,1]::[Word8])),
							(bits [2,2,2,2], wordsToCounters ([2,5,2,1]::[Word8])),
							(bits [4,4,4,4], wordsToCounters ([3,9,3,1]::[Word8]))]

findAddresses addresses target = filterKey (\b -> (hamming'' b $ bits target) < range) addresses

readAddress addresses target = map fromCounters (chunksOf 8 $ sumWords . M.elems $ findAddresses addresses target)

fromCounter :: (Num a, Num a1, Ord a, Bits a1) => a -> Int -> a1
fromCounter c i
	| c > 0		= Bits.bit i
	| otherwise = 0

fromCounters counts =  foldl1 (.|.) bitlist
	where bitlist = map (uncurry fromCounter) taggedcounts
		where taggedcounts = zip (reverse counts) [0..length counts -1]
--filter example
--M.filterWithKey (\k a -> k == (bits [1,1,1,1])) hardAddresses