{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Sp.Gen (
  Sp(..)
, (.+.)
, (.*.)
, (<*.)
, biparB
, biparL
, findLevelIndex
, fromIndexInPartitionB
, findPartitionFromIndex
, findMulPartitionFromIndex
, mul
, one
, set
, dropLast
, ckn
, fac
) where

import           Control.Applicative
import           Control.Arrow (first, second)
import           Data.Maybe
import           Data.List
import           Prelude

data Sp a c = Sp {
  -- enumerate all the structures from a given set of labels
  enum :: [a] -> [c]
  -- cardinality of the list of structures having the same size
, card :: Integer -> Integer
  -- return the structure corresponding to a given size and given label
  -- the list of labels must have enough labels to build that structure
, fromIndex
     :: Integer -- size
     -> Integer -- index
     -> [a]     -- labels
     -> Maybe c -- structure
}

type BiPar a = Sp a ([a], [a])

(.+.) :: Sp a b -> Sp a c -> Sp a (Either b c)
(.+.) (Sp e1 c1 i1) (Sp e2 c2 i2) = Sp e c i
  where
    e us = (Left <$> e1 us) ++ (Right <$> e2 us)
    c n = c1 n + c2 n
    i k n us
      | k < c1 n  = Left <$> i1 k (c1 n) us
      | otherwise = Right <$> i2 k (c2 n) us

(.*.) :: Sp a b -> Sp a c -> Sp a (b, c)
(.*.) (Sp e1 c1 i1) (Sp e2 c2 i2) = Sp e c i
    where
      e us = enum biparB us >>= \(vs, zs) -> (,) <$> e1 vs <*> e2 zs

      c n = let cbl = card biparB n
             in sum $ (\k -> c1 k * c2 (cbl - k)) <$> [1..cbl]

      i n k us =
        do
          ((vs, zs), j) <- findMulPartitionFromIndex n k c1 c2 us
          let (lv, lz) = (toInteger . length $ vs, toInteger . length $ zs)
          let (b, p) = j `quotRem` max 1 (lv * lz)
          let vs1 = i1 lv b vs
              zs1 = i2 lz p zs
           in  (,) <$> vs1 <*> zs1

-- | simple linear search
findMulPartitionFromIndex
  :: Integer  -- number of elements
  -> Integer  -- index .*.
  -> (Integer -> Integer) -- card1
  -> (Integer -> Integer) -- card2
  -> [a]                  -- labels
  -> Maybe (([a], [a]), Integer)  -- result
findMulPartitionFromIndex n k c1 c2 us =
  go 0 k
  where go l j =
          do (p1, p2) <- fromIndex biparB n l us
             let c = max (c1 (toInteger $ length p1) * c2 (toInteger $ length p2)) 1
             if j < c then return ((p1, p2), j)
             else go (l + 1) (j - c)


findPartitionFromIndex :: Integer -> Integer -> [a] -> Maybe ([a], [a])
findPartitionFromIndex n k us
  | k >= card biparB n = Nothing
  | n == 0 = Nothing
  | otherwise =
      let labels = take (fromInteger n) us
          final = last labels
      in  if k < card biparB (n - 1) then
            do
              (vs, zs) <- findPartitionFromIndex (n - 1) k (dropLast labels)
              return (vs, zs ++ [final])
          else
            do
              (vs, zs) <- findPartitionFromIndex (n - 1) (k - card biparB (n - 1)) (dropLast labels)
              return (vs ++ [final], zs)

(<*.) :: Sp a b -> Sp a c -> Sp a (b, c)
(<*.) (Sp e1 c1 i1) (Sp e2 c2 i2) = Sp e c i
    where
      e us = enum biparL us >>= \(vs, zs) -> (,) <$> e1 vs <*> e2 zs

      c n = let cbl = card biparL n
             in sum $ (\k -> c1 k * c2 (cbl - k)) <$> [1..cbl]

      i n k us =
        do
          (l, j)   <- findLevelIndex n k (\v -> c1 v * c2 (n-v)) -- l is the size of the first partition
          let (vs, zs) = splitAt (fromInteger l) us
          -- j is the index of the product based on that length
          -- there are card biparL l such products and each has (c1 l * c2 (n-l)) possibilities
          let (p, q) = j `quotRem` (c1 l * c2 (n-l))
          let vs1 = i1 l p vs
              zs1 = i2 (n - l) q zs
           in  (,) <$> vs1 <*> zs1


fromIndexInPartitionB
  :: Integer          -- full size
  -> Integer          -- size of the smallest partition
  -> Integer          -- index
  -> [a]              -- set of labels
  -> Maybe ([a], [a]) -- result
fromIndexInPartitionB n l k us
  | k >= ckn l n = Nothing
  | n == 0 = Just ([], [])
  | l == n = Just (take (fromInteger n) us, [])
  | l == 0 = Just ([], take (fromInteger n) us)
  | otherwise =
     let labels = take (fromInteger n) us
         final = last labels
     in  if k < ckn l (n - 1) then
           do
             (vs, zs) <- fromIndexInPartitionB (n - 1) l k (dropLast labels)
             return (vs, zs ++ [final])
         else
           do
             (vs, zs) <- fromIndexInPartitionB (n - 1) (l - 1) (k - ckn l (n - 1)) (dropLast labels)
             return (vs ++ [final], zs)

dropLast :: [a] -> [a]
dropLast = reverse . drop 1 . reverse

ckn :: Integer  -> Integer -> Integer
ckn k n = product [(n-k+1)..n] `quot` fac k

fac :: Integer -> Integer
fac n = if n <= 1 then 1 else n * fac (n - 1)


-- for a given index, size and cardinality function find
-- the level and the index of the level
findLevelIndex
  :: Integer                  -- level
  -> Integer                  -- index
  -> (Integer -> Integer)     -- cardinality function
  -> Maybe (Integer, Integer) --
findLevelIndex n k c = go k 0
  where go j l
         | l > n     = Nothing
         | j < c l   = Just (l, j)
         | otherwise = go (j - c l) (l + 1)



biparB :: BiPar a
biparB = Sp e c i
  where
    e [] = [([], [])]
    e (u:ut) = e ut >>= \(vs, zs) -> [(vs, u:zs), (u:vs, zs)]

    c n = 2 ^ n

    i l j us@(u:ut)
      | l == 0 = i j l us
      | l == 1 && j == 0 = Just ([], [u])
      | l == 1 && j == 1 = Just ([u], [])
      | even j           = second ((:) u) <$> i (l - 1) (j `quot` 2) ut
      | otherwise        = first ((:) u)  <$> i (l - 1) (j `quot` 2) ut
    i 0 0 [] = Just ([], [])
    i _ _ [] = Nothing

biparL :: BiPar a
biparL = Sp e c i
  where
    e [] = [([], [])]
    e us@(u:ut) = ([], us) : [ (u:vs, zs) | (vs, zs) <- enum biparL ut ]

    c n = n + 1

    i l j (u:ut)
      | l == 0 && j == 0 = Just ([], [])
      | l == 0           = Nothing
      | l == 1 && j == 0 = Just ([], [u])
      | l == 1 && j == 1 = Just ([u], [])
      | j == 0           = Just ([], take (fromInteger l) (u:ut))
      | otherwise        = first ((:) u) <$> i (l - 1) (j - 1) ut
    i 0 0 [] = Just ([], [])
    i _ _ [] = Nothing

mul :: BiPar a -> Sp a b -> Sp a c -> Sp a (b,c)
-- mul h f g us = h us >>= \(vs,zs) -> (,) <$> f vs <*> g zs
mul (Sp bpe bpc bpi) (Sp e1 c1 i1) (Sp e2 c2 i2) = Sp e c i
  where
    e us = bpe us >>= \(vs, zs) -> (,) <$> e1 vs <*> e2 zs
    c n = bpc n * undefined
    i = undefined


set :: Sp a [a]
set = Sp e c i
  where
    e us = [us]
    c _ = 1

    i _ 0 us = Just us
    i _ _ _  = Nothing


one :: Sp a [b]
one = Sp e c i
  where
    e us = case us of
      []  -> []
      [_] -> [[]]
      _   -> []

    c n = case n of
      0 -> 0
      1 -> 1
      _ -> 0

    i _ 0 (_:_) = Just []
    i _ _ _     = Nothing
