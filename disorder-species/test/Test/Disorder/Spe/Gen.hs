{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Spe.Gen where

import Control.Applicative
import           Disorder.Spe.Gen

import           Data.Foldable
import           Data.Text as T
import           Data.List as L hiding (permutations)

import           Test.QuickCheck

prop_permutations = forAll (permutations simpsons) $ \names ->
  sort names === sort simpsons

prop_klists =
  forAll (elements [1..7]) $ \n ->
  forAll (elements [1..n]) $ \k ->
  forAll (klists k [(1::Int)..n]) $ \l ->
  L.length l === k

prop_lists =
  forAll (elements [1..7]) $ \n ->
  forAll (lists [(1::Int)..n]) $ \l ->
    collect l $
    L.length l <= n

prop_ksets =
  forAll (elements [1..7]) $ \n ->
  forAll (elements [1..n]) $ \k ->
  forAll (ksets k [(1::Int)..n]) $ \s ->
  L.length s === k

prop_sets =
  forAll (elements [1..7]) $ \n ->
  forAll (sets [(1::Int)..n]) $ \s ->
  L.length s <= n

prop_trees =
  forAll (elements [1..7]) $ \n ->
  forAll (trees [(1::Int)..n]) $ \t ->
  (L.length . toList $ t) === n

prop_orderedpairs =
  forAll (orderedPairs [1..(10::Int)]) $
  uncurry (<)

prop_distinct =
  forAll (distinct (arbitrary :: Gen Int)) $ \is ->
  nub is === is

{-
This property should exhaust its generator
distinct_discarded =
  forAll (distinct (elements [1..(1:: Int)])) $ \is ->
  nub is === is
-}

prop_pairs =
  forAll (pairs [1..10 :: Int]) $ \p ->
  uncurry (/=) p

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
