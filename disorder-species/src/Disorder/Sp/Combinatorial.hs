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
, isLastPartitionSet
, nextPartitionSet
, partitionSetToPartition
, unfoldr'
) where

import           Disorder.Sp.ST

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.ST

import           Data.STRef
import           Data.Array.ST
import           Data.Foldable as F (toList)
import           Data.List as L
import qualified Data.Map as M hiding (singleton, (!))
import           Data.Maybe

import           Prelude hiding (pi)

intPartitions :: Int -> [[Int]]
intPartitions n =
  F.toList $ runST $
    do let asr = newArray (0, n + 1) 0
       as     <- asr
       kr     <- newSTRef 1
       result <- newSTRef []
       writeArray as 1 n

       whileM_ (ap1 (/= 0) kr) $
         do k   <- readSTRef kr
            ak1 <- readArray as (k - 1)
            ak  <- readArray as k
            xr  <- newSTRef (ak1 + 1)
            yr  <- newSTRef (ak  - 1)
            kr  -= 1

            whileM_ (ap2 (<=) xr yr) $
              do k' <- readSTRef kr
                 x  <- readSTRef xr
                 writeArray as k' x
                 yr -= x
                 kr += 1
            writeST as kr (ap2 (+) xr yr)
            current <- arrayToList as kr
            result .= (current :)
       val result

firstPartitionSet :: Int -> Int -> [(Int, Int)]
firstPartitionSet n k =
  let ps0 = replicate (n - k + 1) 0 ++ ((\i -> i - (n - k)) <$> [(n - k + 1) .. (n - 1)])
  in  zip ps0 ps0

nextPartitionSet :: Int -> Int -> [(Int, Int)] -> Maybe [(Int, Int)]
nextPartitionSet n k pms =
  if isLastPartitionSet n k pms then Nothing else
    Just $ F.toList $ runSTArray $
    do
      pmsa <- newListArray (0, n - 1) pms :: ST s (STArray s Int (Int,Int))
      forM_ (reverse [1..(n - 1)]) $ \i ->
        do
          (pi, mi)      <- readArray pmsa i
          (_, miMinus1) <- readArray pmsa (i - 1)

          when (pi < k - 1 && pi <= miMinus1) $
            do
              let mi' = max mi (pi + 1)
              writeArray pmsa i (pi + 1, mi')
              forM_ [(i + 1)..(n - (k - mi'))]     $ \j -> writeArray pmsa j (0, mi')
              forM_ [(n - (k - mi') + 1)..(n - 1)] $ \j -> writeArray pmsa j (k - (n - j), k - (n - j))
      return pmsa

isLastPartitionSet :: Int -> Int -> [(Int, Int)] -> Bool
isLastPartitionSet n k pms =
  foldr (\i r ->
      let (pi, _) = pms !! i
          (_, miMinus1) = pms !! (i - 1)
      in  r && (pi >= k - 1 || pi > miMinus1)) True (reverse [1..(n - 1)])

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
ckn2 0 0 = 1
ckn2 k n =
  if k == 0 || n == 0 then 0 else
  ckn2 (k - 1) (n - 1) + k * ckn2 k (n - 1)

-- | Bell number
bell :: Integer -> Integer
bell n = sum $ (`ckn2` n) <$> [0..n]

-- | factorial
fac :: Integer -> Integer
fac n = if n <= 1 then 1 else n * fac (n - 1)

-- | specialisation of unfoldr where the return value is the same as the seed
unfoldr' :: (a -> Maybe a) -> a -> [a]
unfoldr' f a = a : unfoldr (\y -> (id &&& id) <$> f y) a
