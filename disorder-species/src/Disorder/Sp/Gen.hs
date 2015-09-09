{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Sp.Gen (
  Sp(..)
, (.+.)
, (.*.)
, (<*.)
, biparB
, biparL
, ckn
, dropLast
, fac
, findLevelIndex
, fromIndexInPartitionB
, findPartitionFromIndex
, findMulPartitionFromIndex
, idSp
, isOfLength
, list
, mul
, nonEmptyList
, one
, ofSize
, set
, singleton
, x
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

instance Functor (Sp a) where
  fmap f (Sp e1 c1 i1) = Sp e c i
    where e = (f <$>) . e1
          c = c1
          i n k us = f <$> i1 n k us

type BiPar a = Sp a ([a], [a])

(.+.) :: Sp a b -> Sp a c -> Sp a (Either b c)
(.+.) (Sp e1 c1 i1) (Sp e2 c2 i2) = Sp e c i
  where
    e us = (Left <$> e1 us) ++ (Right <$> e2 us)
    c n = c1 n + c2 n
    i n k us
      | k < c1 n  = Left  <$> i1 n k us
      | otherwise = Right <$> i2 n (k - c1 n) us

(.*.) :: Sp a b -> Sp a c -> Sp a (b, c)
(.*.) = mul biparB

(<*.) :: Sp a b -> Sp a c -> Sp a (b, c)
(<*.) = mul biparL

mul :: BiPar a -> Sp a b -> Sp a c -> Sp a (b,c)
mul bipar@(Sp bpe bpc _) sp1 sp2 = Sp e c i
  where
      e us = bpe us >>= \(vs, zs) -> (,) <$> enum sp1 vs <*> enum sp2 zs

      c n = let cbl = bpc n
             in if n == 0 then 1 else sum $ (\k -> card sp1 k * card sp2 (cbl - k)) <$> [1..cbl]

      i n k us =
        do
          ((vs, zs), j) <- findMulPartitionFromIndex n k bipar (card sp1) (card sp2) us
          let (lv, lz) = (toInteger . length $ vs, toInteger . length $ zs)
          let (cv, cz) = (card sp1 lv, card sp2 lz)
          let (b, p) = j `quotRem` (cv * cz)
          let v1 = fromIndex sp1 lv b vs
              z1 = fromIndex sp2 lz p zs
           in  (,) <$> v1 <*> z1

-- | simple linear search
findMulPartitionFromIndex
  :: Integer  -- number of elements
  -> Integer  -- index
  -> BiPar a    -- partition Species
  -> (Integer -> Integer) -- card1
  -> (Integer -> Integer) -- card2
  -> [a]                  -- labels
  -> Maybe (([a], [a]), Integer)  -- result
findMulPartitionFromIndex n k bipar c1 c2 us =
  go 0 k
  where go l j =
          if l > card bipar n then Nothing else
          do (p1, p2) <- fromIndex bipar n l us
             let c = c1 (toInteger $ length p1) * c2 (toInteger $ length p2)
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

list :: Sp a [a]
list = either (const []) id <$> one .+. nonEmptyList

nonEmptyList :: Sp a [a]
nonEmptyList =
  let nel = uncurry (:) <$> x .*. list :: Sp a [a]
      e = enum nel
      c n = if n == 0 then 0 else fac n
      i 0 _ _ = Nothing
      i 1 0 (a:_) = Just [a]
      i n k us = fromIndex nel n k us
  in Sp e c i

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
    e [] = [[]]
    e _ = []

    c n = if n == 0 then 1 else 0

    i 0 0 _ = Just []
    i _ _ _     = Nothing

singleton :: Sp a a
singleton = x

x :: Sp a a
x =
  let e [a] = [a]
      e _ = []

      c n = if n == 1 then 1 else 0

      i 1 0 (a:_) = Just a
      i _ _ _ = Nothing
  in  Sp e c i


idSp :: Sp a a
idSp = Sp e c i
  where
    e = id
    c n = n
    i _ _ = listToMaybe

-- Like length us == n, but lazy.
isOfLength :: [a] -> Integer -> Bool
isOfLength [] n = n == 0
isOfLength (_:us) n = n > 0 && us `isOfLength` (n-1)

-- | f `ofSize` n is like f on n element sets, but empty otherwise.
ofSize :: Sp a c -> Integer -> Sp a c
ofSize (Sp e1 c1 i1) n = Sp e c i
  where
    e us =
      let values = e1 us
      in  if values `isOfLength` n then values
          else []

    c k = if k == n then c1 k else 0

    i l k us = if l == n then i1 l k us else Nothing
