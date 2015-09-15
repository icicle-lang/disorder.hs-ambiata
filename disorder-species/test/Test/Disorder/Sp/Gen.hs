{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Sp.Gen where

import           Control.Applicative
import           Disorder.Sp.Gen

import qualified Data.Text as T

import           Test.QuickCheck

cardinality (Sp e c _) as =
  label "cardinality" $
  length (e as) === fromInteger (c (toInteger $ length as))

indexing (Sp e _ i) (Positive k) as =
  label "indexing" $
  let values = e as
  in i (toInteger $ length as) k as ===
     if k >= toInteger (length values) then Nothing
     else Just (values !! fromInteger k)

fastIndexing sp@(Sp _ _ i) (Positive k) as =
  label "fast indexing" $
  let values = fenum sp as
  in i (toInteger $ length as) k as ===
     if k >= toInteger (length values) then Nothing
     else Just (values !! fromInteger k)

species :: (Arbitrary PositiveSmall, Arbitrary (Positive Integer), Eq b, Show b) =>
           Sp Int b -> PositiveSmall -> Positive Integer -> Property
species sp (PositiveSmall n) k =
  conjoin [
    cardinality  sp [1..n]
  , indexing     sp k [1..n]
  , fastIndexing sp k [1..n]
  ]

prop_findLevelIndex (Positive n) (Positive k) (Positive a)=
  let c m = a * m
  in  case findLevelIndex n k c of
        Nothing     -> property True
        Just (l, j) -> sum (c <$> [0..(l-1)]) + j === k

prop_one = species (one :: Sp Int [Int])

prop_set = species (set :: Sp Int [Int])

prop_biparL = species biparL

prop_biparB = species biparB

prop_kpartitions (PositiveSmall k) = species (kpartitions (toInteger k))

prop_ordMul = species (set <*. set)

prop_mul = species (set .*. set)

prop_nonEmptyList = species nonEmptyList

prop_list = species list

prop_bool = species bool

prop_biparC (PositiveSmall l) = species (biparC (toInteger l))

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll as = all (`elem` as)

data PositiveSmall =
  PositiveSmall Int
  deriving (Eq, Show)

instance Arbitrary PositiveSmall where
  arbitrary = PositiveSmall <$> choose (1, 5)

----------
-- HELPERS
----------

simpsons :: [T.Text]
simpsons =
  [
    "homer"
  , "marge"
  , "maggie"
  , "lisa"
  , "bart"
  , "flanders"
  , "moe"
  , "barney"
  ]
--

return []
tests :: IO Bool
tests = $quickCheckAll
