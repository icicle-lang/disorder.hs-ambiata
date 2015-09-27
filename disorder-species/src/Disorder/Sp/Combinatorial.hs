{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Sp.Combinatorial (
  bell
, cnk
, cnk2
, fac
, intPartitions
, kintPartitions
, ksetPartitions
, kintPartitionsCard
, kintPartitionToWords
, wordToPartition
, intPartitionsCard
, setPartitions
, unfoldr1
, unfoldMany
) where

import           Control.Applicative
import           Control.Arrow

import           Data.Function.Memoize
import           Data.List as L
import qualified Data.Map as M hiding (singleton, (!))
import           Data.Maybe

import           Prelude hiding (pi, read)

-- | enumerate the integer partitions of a number n
--   in order of partition size (number of summands of n)
intPartitions :: Integer -> [[Integer]]
intPartitions n = concatMap (kintPartitions n) [1..n]

-- | enumerate the number of way to decompose a number n
--   in order k parts
kintPartitions :: Integer -> Integer -> [[Integer]]
kintPartitions = memoize2 kintPartitions_
  where
    kintPartitions_ :: Integer -> Integer -> [[Integer]]
    kintPartitions_ n k
      | n < 0 || k < 0 = []
      | k > n = []
      | k == 1 = [[n]]
      | otherwise =
          ((1:) <$> kintPartitions (n - 1) (k - 1)) ++
          (map (+1) <$> kintPartitions (n - k) k)

-- | enumerate all the set partitions of a set of a given size n
--   ordered by partitions size (number of subsets in the partition)
setPartitions :: Integer -> [[[Integer]]]
setPartitions n = concatMap (ksetPartitions n) [1..n]

-- | enumerate all the set partitions of a set of a given size n
--   with k subsets.
--
--   Actually "words" are being enumerated rather than set partitions
--   A "word" is an array representation of a set partition where
--   the "letter" j at position i indicates that element i belongs
--   to the subset j.
--   Subsets are being ordered by the elements having the lowest index
--   For example: [[1,3,5][2,4][6,7]] is set partition of [1..7]
--   it is represented by the word [0,1,0,1,0,2,2]
--   One characteristic of this "word" representation is that
--   for i >= 2  k[i] <= max k[j] + 1 for j < i
--   Informally this means that the value at the "next index" can not be
--   "too big" compared to previous values. This is why this is called
--   a condition of "restricted growth"
--
--   To enumerate set partitions of size k we need to
--     - enumerate the int partitions of size k
--     - for each int partition, find all the words
--       having the same number of elements than the summands in the int
--       partition
ksetPartitions :: Integer -> Integer -> [[[Integer]]]
ksetPartitions n k =
  let ints = kintPartitions n k
  in  wordToPartition <$> concatMap kintPartitionToWords ints

-- | type alias to help with helper functions
-- integer partition assumes that the numbers are sorted
type IntegerPartition = [Integer]
type SetPartition = [[Integer]]
-- Word assumes that the numbers are sorted
type Word = [Integer]
type Letter = Integer

-- | for a given int partition of size k
--   enumerate all the words representing k sets
--   so that the number of elements in each set
--   corresponds to the summands of the int partition
kintPartitionToWords :: IntegerPartition -> [Word]
kintPartitionToWords ip =
    unfoldMany (stepWord n k ip) []
    where
      n = sum ip
      k = toInteger (length ip)

-- | for a given integer partition and current word prefix
--   find the possible extensions for that word which are compatible with
--   the int partition. If no more extensions can be found return either
--   a word that corresponds to that partition or nothing
stepWord :: Integer -> Integer -> IntegerPartition -> Word -> Either [Word] (Maybe Word)
-- no more ints in the partition
-- the result is valid if it has n elements
stepWord n _ [] as
  | length as == fromInteger n = Right (Just as)
  | otherwise                  = Right Nothing

stepWord _ _ _ [] = Left [[0]]

-- if the current word is finished return it
-- if it can not be finished given the current int partition, return nothing
-- otherwise, find out the possible letters extending that word (see the restricted growth property)
-- and return those words
stepWord _ k is as
  | wordIsFinished is as = Right (Just as)
  | wordCantBeFinished is as = Right Nothing
  | otherwise =
          -- get the maximum index value in the existing word
      let m = maximum as
          -- the candidates are all the possible indices which are not too big
          candidates = filter (<= m + 1) [0..(k-1)]
      in  Left $ catMaybes $ nextWord is as <$> candidates

-- | try to extend the word with another letter
--   this is only possible if there are enough
--   "slots" left in the int partition.
nextWord :: IntegerPartition -> Word -> Letter -> Maybe Word
nextWord ip w letter =
  let next = w ++ [letter]
      minus = subIntPartition ip next
  in  const next <$> minus

-- | A word is finished if the set partition it represents
--   has as many elements as the int partition and if each set
--   size corresponds to a summand in the int partition
wordIsFinished :: IntegerPartition -> Word -> Bool
wordIsFinished ip w =
  length w == fromInteger (sum ip) &&
  fromMaybe False (all (== 0) <$> subIntPartition ip w)

-- | A word is finished if the set partition it represents
--   has as many elements as the int partition but if some set
--   size does not correspond to a summand in the int partition
wordCantBeFinished :: IntegerPartition -> Word -> Bool
wordCantBeFinished ip w =
  length w == fromInteger (sum ip) &&
  fromMaybe False (any (> 0) <$> subIntPartition ip w)

-- | for a given int partition and word subtract the word sizes
--   from the int partition number and return the remaining letters which can
--   be used to extend the word.
subIntPartition :: IntegerPartition -> Word -> Maybe IntegerPartition
subIntPartition ip w =
  let ws = wordSizes w                  -- pad the sizes with zeros
      s = uncurry (-) <$> zip (reverse ip) (ws ++ replicate (length ip - length w) 0)
  in if all (>= 0) s then Just s else Nothing

-- | compute the number of times that each letter of the word is represented
wordSizes :: Word -> IntegerPartition
wordSizes w =
  sortBy (flip compare) ((toInteger . length) <$> group (sort w))

-- | return the number of int partitions of n
intPartitionsCard :: Integer -> Integer
intPartitionsCard n = sum (kintPartitionsCard n <$> [1..n])

-- | return the number of int partitions of n
--   having k summards
kintPartitionsCard :: Integer -> Integer -> Integer
kintPartitionsCard =
  memoize2 kintPartitionsCard_
  where
    kintPartitionsCard_ n k
      | n < 0 || k < 0 = 0
      | k == 1 = 1
      | k > n = 0
      | otherwise = kintPartitionsCard (n - 1) (k - 1) + kintPartitionsCard (n - k) k

-- |
wordToPartition :: Word -> SetPartition
wordToPartition ps =
  (\m -> snd <$> M.toList m) $ L.foldr updateSet M.empty (zip ps [(0::Integer)..])
  where
    updateSet (pj, j) m =
      let s = M.findWithDefault [] pj m
      in  M.insert pj (j:s) m

-- | Binomial number
cnk :: Integer  -> Integer -> Integer
cnk n k = product [(n-k+1)..n] `quot` fac k

-- | Second Stirling number
cnk2 :: Integer  -> Integer -> Integer
cnk2 = memoize2 cnk2_
  where
    cnk2_ 0 0 = 1
    cnk2_ n k =
      if k == 0 || n == 0 then 0 else
      cnk2 (n - 1) (k - 1) + k * cnk2 (n - 1) k

-- | Bell number
--
bell :: Integer -> Integer
bell n = sum $ cnk2 n <$> [0..n]

-- | factorial
fac :: Integer -> Integer
fac n = if n <= 1 then 1 else n * fac (n - 1)

-- | unfold search function
--   from a the current element
--   compute either:
--     - a list of possible next candidates
--     - or the final solution
--     - or nothing if the current element was a dead end
unfoldMany :: (a -> Either [a] (Maybe a)) -> a -> [a]
unfoldMany f a =
  case f a of
    Left others -> concatMap (unfoldMany f) others
    Right (Just found) -> [found]
    Right Nothing -> []

-- | specialisation of unfoldr where the return value is the same as the seed
unfoldr1 :: (a -> Maybe a) -> a -> [a]
unfoldr1 f a = a : unfoldr (\y -> (id &&& id) <$> f y) a
