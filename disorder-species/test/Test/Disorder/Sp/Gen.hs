{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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

species sp k as =
  cardinality sp as .&&. indexing sp k as

prop_findLevelIndex (Positive n) (Positive k) (Positive a)=
  let c m = a * m
  in  case findLevelIndex n k c of
        Nothing     -> property True
        Just (l, j) -> sum (c <$> [0..(l-1)]) + j === k

prop_one = species (one :: Sp Int [Int])

prop_set (PositiveSmall n) k = species (set :: Sp Int [Int]) k [1..n]

prop_biparL (PositiveSmall n) k = species biparL k [1..n]

prop_biparB (PositiveSmall n) k = species biparB k [1..n]

prop_ordMul (PositiveSmall n) k = species (set <*. set) k [1..n]

prop_mul (PositiveSmall n) k = species (set .*. set) k [1..n]

prop_fromIndexInPartitionB (PositiveSmall n) (Positive k) = forAll (choose (0, n)) $ \l ->
  let values = filter ((== l) . length . fst) (enum biparB [1..n]) :: [([Int], [Int])]
      actual = fromIndexInPartitionB (toInteger n) (toInteger l) (toInteger k) [1..]
  in  if k >= length values then actual === Nothing
      else                       actual === Just (values !! k)

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll as = all (`elem` as)

data PositiveSmall =
  PositiveSmall Int
  deriving (Eq, Show)

instance Arbitrary PositiveSmall where
  arbitrary = PositiveSmall <$> choose (1, 10)

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
