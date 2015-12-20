{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Word
import Data.Bits as Bits
import Data.List.Split


-- Constants for testing purposes
range :: Int
range = 4

x :: Data.Word.Word8
x = 1


-- List utilities

-- This is a substitute for vector addition
listSum :: (Num a) => [a] -> [a] -> [a]
listSum = zipWith (+)


-- Conversion to SDM 'counter format

-- Converts bit at given position to 1 or -1 depending
counter :: (Num a1, Bits a) => a -> Int -> a1
counter a b
	| Bits.testBit a b = 1
	| otherwise        = -1

-- Mapped version with appropriate reverse. Takes bit datatype and converts it. We want the highest valued bits to be and the
-- left, or beginning of the list
-- This is the kind of thing better represented as a bit vector. Or something with a map instance at least
bitsToCounters :: (Num t, Bits a) => a -> [t]
bitsToCounters thebits = [counter thebits x| x <- reverse [0..(Bits.bitSize thebits - 1)]] 

-- The 'words' in the SDM are represented by a list of types satisfying Bits a. We don't actually care about the chunk boundaries
-- so we concatenate after mapping. Hence concatMap
wordsToCounters :: (Num b, Bits a) => [a] -> [b]
wordsToCounters x = concatMap bitsToCounters x

-- Convert given 'counter' value to Bits with that bit set/unset
fromCounter :: (Num a, Num a1, Ord a, Bits a1) => a -> Int -> a1
fromCounter c i
	| c > 0		= Bits.bit i
	| otherwise = 0

-- This one does most of the work. This is what right justifies the bits in addition to handling words, etc
fromCounters counts =  foldl1 (.|.) bitlist
	where bitlist = map (uncurry fromCounter) taggedcounts
		where taggedcounts = zip (reverse counts) [0..length counts -1]

-- Shorthand for summing words. If we start with a two lists of Bits a.
-- Should test to say if I can leave the signature as a generic. I recall there being some issue later. Might be fixable if I do NoMonomorphismRestriction
sumWords :: [[Word8]] -> [Word8]
sumWords wordlist = foldl1  listSum wordlist


-- Unimplemented The idea was to implement a map for Bytestrings or just Bits a. The given Bool -> b would be a proxy for {0,1} -> {0,1}
mapByte :: (Bits.Bits a) => (Bool -> b) -> a -> [b] 
mapByte = undefined

-- Utilities for dealing with Map. The point of result is to convert found keys to a bunch of elements and deal with unpacking. I am 90% certain this is something to
-- axe even before moving to a smarter implementation

-- Wrapper around filter key because we are _only_ filtering on keys and don't want the extra junk associated with identity filters for
-- elements all the time
filterKey predicate source = M.filterWithKey (\k a -> predicate k) source

-- Hack of a hamming distance from bits
hamming ::  Bits a => a -> a -> Int
hamming a b = Bits.popCount (Bits.xor a b)

-- THIS IS HAMMING FOR WORDS. Super unclear
hamming' :: Bits b => [b] -> [b] -> Int
hamming' a b = sum $ zipWith hamming a b

-- Need to rename at least. Pretty sure I need to either use bytestrings better or just drop them.
bits :: [Data.Word.Word8] -> B.ByteString
bits = B.pack

-- Addresses for testing
hardAddresses :: M.Map [Word8] [Word8]
hardAddresses = M.fromList [([0,0,0,0], wordsToCounters ([0,0,0,1]::[Word8])),
							( [1,1,1,1], wordsToCounters ([1,3,1,1]::[Word8])),
							( [2,2,2,2], wordsToCounters ([2,5,2,1]::[Word8])),
							( [4,4,4,4], wordsToCounters ([3,9,3,1]::[Word8]))]

-- Filterkey based on standard hamming distance. Gives returned addresses, necessary for read/write ops.
findAddresses :: M.Map [Word8] [Word8] -> [Word8] -> M.Map [Word8] [Word8]
findAddresses addresses target = filterKey (\b -> (hamming' b target) < range) addresses

-- Read that aggregates
readAddress addresses target = map fromCounters (chunksOf 8 $ sumWords . M.elems $ findAddresses addresses target)


--filter example
--M.filterWithKey (\k a -> k == (bits [1,1,1,1])) hardAddresses
