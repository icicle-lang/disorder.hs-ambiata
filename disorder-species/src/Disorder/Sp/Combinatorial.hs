{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Sp.Combinatorial (
  bell
, ckn
, ckn2
, fac
, firstPartitionSet
, intPartitions
, kintPartitions
, nextPartitionSet
, partitionSetToPartition
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
import           Data.Foldable as F (toList)
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
        step [x1] = Nothing

        step (x1:x2:xs)
          | x1 > x2 + 1 = Just (x1 - 1 : x2 + 1 : xs)
          | x1 == x2 = (x1:) <$> step (x2:xs)
          | x1 == x2 + 1 =
             let (minusOne, minusMore) = partition (\x -> x + 1 == x1) (x2:xs)
             in  case minusMore of
                   [] -> Nothing
                   (x3:rest) -> Just $ (x1 - 1) : minusOne ++ (x3 + 1 : rest)

firstPartitionSet :: Int -> Int -> ([Int], [Int])
firstPartitionSet n k =
  let ps0 = replicate (n - k + 1) 0 ++ ((\i -> i - (n - k)) <$> [(n - k + 1) .. (n - 1)])
  in  (ps0, ps0)

-- | see the algorithm described by Michael Orlov
--   http://www.informatik.uni-ulm.de/ni/Lehre/WS03/DMM/Software/partitions.pdf
nextPartitionSet :: Int -> Int -> ([Int], [Int]) -> Maybe ([Int], [Int])
nextPartitionSet n k (ps, ms) =
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

partitionSetToPartition :: [a] -> [Int] -> [[a]]
partitionSetToPartition us ps =
  (\m -> snd <$> M.toList m) $ L.foldr updateSet M.empty (zip ps [(0::Int)..])
  where
    updateSet (pj, j) m =
      let s = M.findWithDefault [] pj m
      in  M.insert pj ((us !! j):s) m

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
