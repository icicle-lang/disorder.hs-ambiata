{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Spe.Gen (
  distinct
, klists
, ksets
, lists
, orderedPairs
, pairs
, permutations
, reservoirSample
, sampleSpecies
, sets
, trees
, treesSpecies
) where

import           Control.Applicative hiding (empty)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class

import           Data.Foldable
import           Data.List as L hiding (permutations)
import           Data.Maybe
import           Data.Sequence as DS
import           Data.Traversable
import           Data.Tree

import           Prelude hiding (length, drop, null)

import           Math.Spe as S

import           Test.QuickCheck

-------------
-- GENERATORS
-------------

-- | Generate lists of distinct elements
permutations ::  [a] -> Gen [a]
permutations = sampleSpecies list

-- | Generate lists of distinct elements of size k
klists :: Int -> [a] -> Gen [a]
klists k as = fst <$> sampleSpecies (list `ofSize` k .*. set) as

-- | Generate lists of distinct elements of size <= length labels
lists :: [a] -> Gen [a]
lists as = fst <$> sampleSpecies (list .*. set) as

-- | Generate sets of distinct elements of size k
ksets :: Int -> [a] -> Gen [a]
ksets k as = fst <$> sampleSpecies (set `ofSize` k .*. set) as

-- | Generate sets of distinct elements of size <= length labels
sets :: [a] -> Gen [a]
sets as = fst <$> sampleSpecies (set .*. set) as

-- | Generate pairs of distinct elements
pairs :: [a] -> Gen (a, a)
pairs as = fst <$> sampleSpecies (x .*. x .*. set) as

-- | Generate ordered pairs
orderedPairs :: [a] -> Gen (a, a)
orderedPairs as = fst <$> sampleSpecies (x <*. x <*. set) as

-- | Generate trees
trees :: [a] -> Gen (Tree a)
trees = sampleSpecies treesSpecies

-- | species of rose trees
treesSpecies :: Spe a (Tree a)
treesSpecies as = (\(n, (_, ns)) -> Node n ns) <$> (x .*. (list `o` treesSpecies)) as

-- | Generate a list of distinct elements
--   this can be used to produce a list of unique labels for species
distinct :: Eq a => Gen a -> Gen [a]
distinct = distinctWithGrowth 1 10

distinctWithGrowth :: Eq a => Int -> Int -> Gen a -> Gen [a]
distinctWithGrowth k m ga =
  if k >= m then discard
  else
    do
      n  <- sized return
      as <- L.take n . nub <$> vectorOf (k * n) ga
      if L.length as < n then distinctWithGrowth (k + 1) m ga
      else                    return as

-- | Generate elements from a species, given a set of labels
--   using reservoir sampling to select a structure randomly
--   this returns a sized generator which will draw elements from a larger
--   set of generated structures as the size grows
sampleSpecies :: S.Spe a b -> [a] -> Gen b
sampleSpecies s ls =
  let species = s ls
  in  do
        n  <- sized return
        as <- reservoirSample 1 (L.take (n + 1) species)
        maybe discard return $ listToMaybe as


---------
-- RANDOM
---------

-- | reservoir sampling from a list of elements
reservoirSample :: Int -> [a] -> Gen [a]
reservoirSample n as
  | n <= 0 = return []
  | otherwise =
      toList . fst <$> execStateT (traverse (sampleState n) as) (empty, 0)

sampleState :: Int -> a -> StateT (Seq a, Int) Gen ()
sampleState size a = do
  (selected, nb) <- get
  _ <- if DS.null selected then put (singleton a, 1)
       else selectNext size (selected, nb) a
  pure ()

selectNext :: Int -> (Seq a, Int) -> a -> StateT (Seq a, Int) Gen ()
selectNext size (as, nb) a = do
  n <- lift $ choose (0, nb + 1)
  let newElements =
        if n + 1 == 1 then
          if DS.length as < size then as |> a
          else DS.drop 1 as |> a
        else as
  put (newElements, nb + 1)
