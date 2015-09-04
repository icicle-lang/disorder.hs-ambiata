{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Sp.Gen (
  Sp(..)
, (.+.)
, biparL
, one
, set
, findLevelIndex
) where

import           Control.Applicative
import           Data.Maybe
import           Prelude ((++), Integer, Either(..), div, (*), (-), (+), (<=), (>), (<), (>=), otherwise, drop, fromInteger, take, splitAt)

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

biparL :: BiPar a
biparL = Sp e c i
  where
    e [] = [([], [])]
    e us@(u:ut) = ([], us) : [ (u:vs, zs) | (vs, zs) <- enum biparL ut ]

    c n = n + 1

    i k n us =
      case findLevelIndex k n c of
        Nothing      -> Nothing
        Just (l, j)  ->
          let values = take (fromInteger l) us
          in  Just (take (fromInteger j) values, drop (fromInteger j) values)

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
