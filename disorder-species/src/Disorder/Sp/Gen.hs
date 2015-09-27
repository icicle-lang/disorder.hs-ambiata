{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Disorder.Sp.Gen (
  Sp(..)
, (.+.)
, (.*.)
, (<*.)
, (><)
, (>.<)
, biparB
, biparC
, biparL
, bool
, fenum
, findLevelIndex
, findMulPartitionFromIndex
, idSp
, isOfLength
, kpartitions
, list
, mul
, nonEmptyList
, o
, one
, ofSize
, partitions
, set
, singleton
, x
) where

import           Disorder.Sp.Combinatorial as C

import           Control.Applicative
import           Control.Arrow (first, second)
import           Control.Monad

import           Data.List as L
import           Data.Maybe

import           Prelude hiding (pi)

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

instance Applicative (Sp a) where
  pure a = const a <$> one
  f <*> s = uncurry ($) <$> (f .*. s)

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

o :: Sp [a] b -> Sp a c -> Sp a (b, [c])
o sp1 sp2 = Sp e c i
  where
    e us = enum partitions us >>= enum sp1 >.< mapM (enum sp2)

    c _ = 0
    i _ _ _ = Nothing

-- | The Cartesian product of two species.
(><) :: Sp a b -> Sp a c -> Sp a (b, c)
(><) f g = (,) <$> f <*> g

-- | The Cartesian product of two enumerations.
(>.<) :: ([a] -> [b]) -> ([a] -> [c]) -> [a] -> [(b, c)]
(>.<) f g us = (,) <$> f us <*> g us

fenum :: Sp a b -> [a] -> [b]
fenum (Sp _ c i) as =
  let las = toInteger . length $ as
  in  concatMap (\k -> maybeToList $ i las (toInteger k) as) [0..(c las - 1)]

-- | The species of set partitions.
partitions :: Sp a [[a]]
partitions = Sp e c i
  where
    e [] = []
    e us =
      let s = toInteger . length $ us
      in  concatMap (\n -> enum (kpartitions n) us) [1..s]

    c n = sum $ cnk2 n <$> [0..n]

    i n k us =
      do
        (l, j) <- findLevelIndex n k (\r -> card (kpartitions r) n)
        fromIndex (kpartitions l) n j us

-- | The species of set partitions having exactly p partitions
kpartitions :: Integer -> Sp a [[a]]
kpartitions k = Sp e c i
  where
    e us =
      let ps = ksetPartitions (toInteger . length $ us) k
      in  mapLabels <$> ps
      where
        mapLabels jss = (\js -> (\j -> us !! fromInteger j) <$> js) <$> jss

    c n = cnk2 n k

    i _ _ _ = Nothing

biparB :: BiPar a
biparB = Sp e c i
  where
    e us =
      concatMap (\l -> enum (biparC (toInteger l)) us) [0..(length us)]

    c n = 2 ^ n

    i n k us =
      do
        (l, j) <- findLevelIndex n k (\r -> card (biparC r) n)
        fromIndex (biparC l) n j us

-- bipartitions of size == l for the first list
biparC :: Integer -> BiPar a
biparC l = Sp e c i
  where
    e [] = [([], []) | l == 0]
    e (u:ut) =
      (first (u:)  <$> enum (biparC (l - 1)) ut) ++
      (second (u:) <$> enum (biparC l) ut)

    c n = cnk n (toInteger l)

    i n k us
      | l > n = Nothing
      | k >= c n = Nothing
      | n == 0 = Just ([], [])
      | l == n = Just (take (fromInteger n) us, [])
      | l == 0 = Just ([], take (fromInteger n) us)
      | otherwise =
         let labels = take (fromInteger n) us
         in  if k < cnk (n - 1) (l - 1) then
               do
                 (vs, zs) <- fromIndex (biparC (l - 1)) (n - 1) k (drop 1 labels)
                 return (head labels : vs, zs)
             else
               do
                 (vs, zs) <- fromIndex (biparC l) (n - 1) (k - cnk (n - 1) (l - 1 )) (drop 1 labels)
                 return (vs, head labels : zs)

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

bool :: Sp a Bool
bool = Sp e c i
  where e []  = [True, False]
        e _   = []

        c n = if n == 0 then 1 else 0

        i 0 0 _ = Just True
        i 0 1 _ = Just False
        i _ _ _ = Nothing

one :: Sp a [b]
one = Sp e c i
  where
    e [] = [[]]
    e _ = []

    c n = if n == 0 then 1 else 0

    i 0 0 _ = Just []
    i _ _ _ = Nothing

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
    e us = if us `isOfLength` n then e1 us else []

    c k = if k == n then c1 k else 0

    i l k us = if l == n then i1 l k us else Nothing

-- for a given index, size and cardinality function find
-- the level and the index of the level
findLevelIndex
  :: Integer                  -- size
  -> Integer                  -- index
  -> (Integer -> Integer)     -- cardinality function
  -> Maybe (Integer, Integer) --
findLevelIndex n k c = go k 0
  where go j l
         | l > n     = Nothing
         | j < c l   = Just (l, j)
         | otherwise = go (j - c l) (l + 1)

-- | simple linear search
findMulPartitionFromIndex
  :: Integer  -- total number of elements
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
             if j < c then
               Just ((p1, p2), j)
             else
               go (l + 1) (j - c)
