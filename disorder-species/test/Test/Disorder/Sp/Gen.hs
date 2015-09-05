{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Sp.Gen where

import           Control.Applicative
import           Disorder.Sp.Gen

import           Data.List
import qualified Data.Text as T

import           Test.QuickCheck

cardinality (Sp e c _) as =
  label "cardinality" $
  length (e as) === fromInteger (c (toInteger $ length as))

indexing (Sp e _ i) (Positive k) as =
  label "indexing" $
  let values = concatMap e $ inits as
  in i k (toInteger $ length as) as ===
    if k >= toInteger (length values) then Nothing
    else Just (values !! fromInteger k)

species sp k as =
  cardinality sp as .&&. indexing sp k as

prop_findLevelIndex (Positive k) (Positive n) (Positive a)=
  let c m = a * m
  in  case findLevelIndex k n c of
        Nothing     -> property True
        Just (l, j) -> sum (c <$> [0..(l-1)]) + j === k

prop_one = species (one :: Sp Int [Int])

prop_set k (PositiveSmall n) = species (set :: Sp Int [Int]) k [1..n]

prop_biparL k (PositiveSmall n) = species biparL k [1..n]

prop_biparB k (PositiveSmall n) = species biparB k [1..n]

data PositiveSmall =
  PositiveSmall Int
  deriving (Eq, Show)

instance Arbitrary PositiveSmall where
  arbitrary = PositiveSmall <$> choose (1, 20)

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
