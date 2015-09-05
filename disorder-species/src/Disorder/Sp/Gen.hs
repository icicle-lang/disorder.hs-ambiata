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
, mul
, one
, set
) where

import           Control.Applicative
import           Control.Arrow (first, second)
import           Data.Maybe
import           Prelude

data Sp a c = Sp {
  enum :: [a] -> [c]
, card :: Integer -> Integer
, index :: Integer -> Integer -> [a] -> Maybe c
}

type BiPar a = Sp a ([a], [a])

(.+.) :: Sp a b -> Sp a c -> Sp a (Either b c)
(.+.) (Sp e1 c1 i1) (Sp e2 c2 i2) = Sp e c i
  where
    e us = (Left <$> e1 us) ++ (Right <$> e2 us)
    c n = c1 n + c2 n
    i k n us =
      if k <= c1 n then Left <$> i1 k (c1 n) us else Right <$> i2 k (c2 n) us

(.*.) :: Sp a b -> Sp a c -> Sp a (b, c)
(.*.) = undefined

(<*.) :: Sp a b -> Sp a c -> Sp a (b, c)
(<*.) (Sp e1 c1 i1) (Sp e2 c2 i2) = Sp e c i
    where
      e us = enum biparL us >>= \(vs, zs) -> (,) <$> e1 vs <*> e2 zs
      c n = let cbl = card biparL n
             in sum $ (\k -> c1 k * c2 (cbl - k)) <$> [1..cbl]
      i = undefined

biparB :: BiPar a
biparB = Sp e c i
  where
    e [] = [([], [])]
    e (u:ut) = e ut >>= \(vs, zs) -> [(vs, u:zs), (u:vs, zs)]

    c n = 2 ^ n

    i 0 0 [] = Just ([], [])
    i _ _ [] = Nothing

    i k n us =
      case findLevelIndex k n c of
        Nothing     -> Nothing
        Just (l, j) -> findOnLevel l j us
      where
      findOnLevel l j (a:at)
        | l == 0 = i j l us
        | l == 1 && j == 0 = Just ([], [a])
        | l == 1 && j == 1 = Just ([a], [])
        | even j =
            second ((:) a) <$> findOnLevel (l - 1) (j `div` 2) at
        | otherwise =
            first ((:) a) <$> findOnLevel (l - 1) (j `div` 2) at
      findOnLevel _ _ [] = Nothing

biparL :: BiPar a
biparL = Sp e c i
  where
    e [] = [([], [])]
    e us@(u:ut) = ([], us) : [ (u:vs, zs) | (vs, zs) <- enum biparL ut ]

    c n = n + 1

    i k n us =
      case findLevelIndex k n c of
        Nothing      -> Nothing
        Just (l, j) -> findOnLevel l j us
      where
      findOnLevel l j (u:ut)
        | l == 0 && j == 0 = Just ([], [])
        | l == 0           = Nothing
        | l == 1 && j == 0 = Just ([], [u])
        | l == 1 && j == 1 = Just ([u], [])
        | j == 0           = Just ([], take (fromInteger l) (u:ut))
        | otherwise        = first ((:) u) <$> findOnLevel (l - 1) (j - 1) ut
      findOnLevel _ _ [] = Nothing

mul :: BiPar a -> Sp a b -> Sp a c -> Sp a (b,c)
-- mul h f g us = h us >>= \(vs,zs) -> (,) <$> f vs <*> g zs
mul (Sp bpe bpc bpi) (Sp e1 c1 i1) (Sp e2 c2 i2) = Sp e c i
  where
    e us = bpe us >>= \(vs, zs) -> (,) <$> e1 vs <*> e2 zs
    c n = (bpc n) * undefined
    i = undefined



-- for a given index, size and count function find
-- the level and the index of the level
findLevelIndex :: Integer -> Integer -> (Integer -> Integer) -> Maybe (Integer, Integer)
findLevelIndex k n c = go k 0
  where go j l
         | l > n     = Nothing
         | j < c l   = Just (l, j)
         | otherwise = go (j - c l) (l + 1)

set :: Sp a [a]
set = Sp e c i
  where
    e us = [us]
    c _ = 1

    i k n us =
      if k < 1 + n  then
        Just (take (fromInteger k) us)
      else
        Nothing


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

    i 0 _ (_:_) = Just []
    i _ _ _     = Nothing
