{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Sp.Combinatorial (
  bell
, ckn
, ckn2
, fac
, intPartitions
, kintPartitions
, ksetPartitions
, kintPartitionsCard
, intPartitionsCard
, firstPartitionWord
, nextPartitionWord
, partitionWordToPartition
, setPartitions
, unfoldr'
) where

import           Disorder.Sp.ST

import           Control.Applicative
import           Control.Arrow
import           Control.Cond
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.ST

import           Data.STRef
import           Data.Array.ST
import           Data.Function.Memoize
import           Data.List as L
import qualified Data.Map as M hiding (singleton, (!))
import           Data.Maybe

import           Prelude hiding (pi, read)

intPartitions :: Integer -> [[Integer]]
intPartitions n = concatMap (kintPartitions n) [1..n]

kintPartitions :: Integer -> Integer -> [[Integer]]
kintPartitions n k =
  let start = n - k + 1  : (const 1 <$> [0..(k - 2)])
  in  unfoldr' step start
  where step :: [Integer] -> Maybe [Integer]
        step [] = Nothing
        step [_] = Nothing
        step (x1:x2:xs)
          | x1 > x2 + 1 = Just (x1 - 1 : x2 + 1 : xs)
          | x1 == x2 = (x1:) <$> step (x2:xs)
          | x1 == x2 + 1 =
             let (minusOne, minusMore) = partition (\x -> x + 1 == x1) (x2:xs)
             in  case minusMore of
                   [] -> Nothing
                   (x3:rest) -> Just $ (x1 - 1) : minusOne ++ (x3 + 1 : rest)
          | otherwise = Nothing

setPartitions :: Integer -> [[[Integer]]]
setPartitions n = concatMap (ksetPartitions n) [1..n]

ksetPartitions :: Integer -> Integer -> [[[Integer]]]
ksetPartitions n k =
  let  pms = firstPartitionWord n k
  in  partitionWordToPartition . fst <$> unfoldr' (nextPartitionWord n k) pms

-- for a given partition return all the possible combinations
-- of partition words
-- for example for 3 1 1 return
-- group by size
-- a * 3 + b * 2 + c * 1
-- c k a <*> c (n - k) b <*> c (n - k) c
-- 0 1 2
-- 0 2 1
-- 1 2 0
-- 000 1 2
-- 0 111 2
-- 0 1 222

-- 3 1 1 - 3 1 = 0 0 1
-- [0,0,0,1,2]
--
-- [0,0,1,0,2]
-- [0,0,1,2,0]
--
-- [0,1,0,0,2]
-- [0,1,0,2,0]
--
-- [0,1,2,0,0]
--
-- [0,1,1,1,2]
-- [0,1,1,2,1]
-- [0,1,2,1,1]
--
-- [0,1,2,2,2]
{-kintPartitionToWords :: [Integer] -> [[Integer]]
kintPartitionToWords partition =
  let k = length partition in
  go 0 partition []
  where
    go i [] r = r
    go i [a] r =

-}

intPartitionsCard :: Integer -> Integer
intPartitionsCard n = sum (kintPartitionsCard n <$> [1..n])

kintPartitionsCard :: Integer -> Integer -> Integer
kintPartitionsCard =
  memoize2 kintPartitionsCard_
  where
    kintPartitionsCard_ n k
      | n < 0 || k < 0 = 0
      | k == 1 = 1
      | k > n = 0
      | otherwise = kintPartitionsCard (n - 1) (k - 1) + kintPartitionsCard (n - k) k

firstPartitionWord :: Integer -> Integer -> ([Integer], [Integer])
firstPartitionWord n k =
  -- first n - k elements in the first set
  -- then all other k elements in a distinct set
  let ps0 = replicate (fromInteger $ n - k + 1) 0 ++ ((\i -> i - (n - i)) <$> [(n - k + 1) .. (n - 1)])
  in  (ps0, ps0)

-- | see the algorithm described by Michael Orlov
--   http://www.informatik.uni-ulm.de/ni/Lehre/WS03/DMM/Software/partitions.pdf
nextPartitionWord :: Integer -> Integer -> ([Integer], [Integer]) -> Maybe ([Integer], [Integer])
nextPartitionWord ni k (ps, ms) =
  let n = fromInteger ni in
  runST $
  -- initialisation
  do result   <- newSTRef Nothing
     hasNext  <- newSTRef False
     ir       <- newSTRef (n - 1)
     let psr =  newListArray (0, n) ps
     let msr =  newListArray (0, n) ms
     psa     <- psr
     msa     <- msr

     -- ir goes from n - 1 to 1 unless we found a next element
     whileM_ (ap2 (\vi vn -> (vi >= 1) && not vn) ir hasNext) $
       do i   <- readSTRef ir
          pi  <- readArray psa i
          mi  <- readArray msa i
          mi1 <- readArray msa (i - 1)
          when (pi < k - 1 && pi <= mi1) $
            do
               hasNext =: True
               let mi' = max mi (pi + 1)
               writeArray psa i (pi + 1)
               writeArray msa i mi'
               forM_ [(i + 1)..(n - (k - mi'))] $ \j -> writeArray psa j 0 >> writeArray msa j mi'
          ir -= 1

     whenM (ap1 (== True) hasNext) $
       do ps1 <- arrayToList' psa (n - 1)
          ms1 <- arrayToList' msa (n - 1)
          result =: Just (ps1, ms1)
     val result

partitionWordToPartition :: [Integer] -> [[Integer]]
partitionWordToPartition ps =
  (\m -> snd <$> M.toList m) $ L.foldr updateSet M.empty (zip ps [(0::Integer)..])
  where
    updateSet (pj, j) m =
      let s = M.findWithDefault [] pj m
      in  M.insert pj (j:s) m

-- | Binomial number
ckn :: Integer  -> Integer -> Integer
ckn k n = product [(n-k+1)..n] `quot` fac k

-- | Second Stirling number
ckn2 :: Integer  -> Integer -> Integer
ckn2 = memoize2 ckn2_
  where
    ckn2_ 0 0 = 1
    ckn2_ k n =
      if k == 0 || n == 0 then 0 else
      ckn2 (k - 1) (n - 1) + k * ckn2 k (n - 1)

-- | Bell number
--
bell :: Integer -> Integer
bell n = sum $ (`ckn2` n) <$> [0..n]

-- | factorial
fac :: Integer -> Integer
fac n = if n <= 1 then 1 else n * fac (n - 1)

-- | specialisation of unfoldr where the return value is the same as the seed
unfoldr' :: (a -> Maybe a) -> a -> [a]
unfoldr' f a = a : unfoldr (\y -> (id &&& id) <$> f y) a
