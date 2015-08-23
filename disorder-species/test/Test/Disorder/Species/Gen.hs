{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Species.Gen where

import           Disorder.Species.Gen

import           Control.Applicative

import           Data.Text as T
import           Data.List as L hiding (permutations)

import qualified Math.Combinatorics.Species as S hiding (elements)

import           Test.QuickCheck

prop_permutations = forAll (permutations simpsons) $ \names ->
  sort names === sort simpsons

prop_subsets = forAll (subsets simpsons) $ \names ->
  conjoin (fmap (`L.elem` simpsons) names)

prop_ksubsets = forAll (choose (0, L.length simpsons)) $ \k ->forAll (ksubsets k simpsons) $ \names ->
  conjoin (fmap (`L.elem` simpsons) names) .&&. (L.length names === k)

prop_pairs = forAll (subsets [1..5]) $ \ls ->
  (sort (fromPairSpeciesStructure <$> S.enumerate pairsSpecies ls) :: [(Int, Int)]) === makePairs ls

prop_orderedPairs = forAll (subsets [1..5]) $ \ls ->
  sort ((fromOrderedPairSpeciesStructure <$> S.enumerate orderedPairsSpecies ls) :: [(Int, Int)]) === makeOrderedPairs ls

----------
-- HELPERS
----------

makePairs :: Eq a => [a] -> [(a, a)]
makePairs as = L.concatMap (\a -> fmap (a,) (L.filter (/= a) as)) as

makeOrderedPairs :: Eq a => [a] -> [(a, a)]
makeOrderedPairs [] = []
makeOrderedPairs (a : as) = fmap (a,) as ++ makeOrderedPairs as

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
