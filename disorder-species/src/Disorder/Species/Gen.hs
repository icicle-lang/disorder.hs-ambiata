{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Species.Gen (
  fromOrderedPairSpeciesStructure
, fromPairSpeciesStructure
, fromSpecies
, ksubsets
, orderedPairs
, orderedPairsSpecies
, pairs
, pairsSpecies
, permutations
, reservoirSample
, subsets
, trees
, treesSpecies
) where

import           Control.Applicative hiding (empty)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class

import           NumericPrelude.Base

import           Data.Traversable
import           Data.Tree
import           Data.Typeable
import           Data.Sequence as DS
import           Data.Foldable

import           Prelude hiding (length, drop, null, (*))
import           Algebra.Ring as Ring

import qualified Math.Combinatorics.Species as S
import           Math.Combinatorics.Species.Structures

import           Test.QuickCheck

-------------
-- GENERATORS
-------------

-- | Generate lists of distinct elements
permutations :: (Eq a, Typeable a) => [a] -> Gen [a]
permutations = fromSpecies S.linOrds

-- | Generate subsets
subsets :: (Eq a, Typeable a) => [a] -> Gen [a]
subsets as = getSet <$> fromSpecies S.subset as

-- | Generate subsets of size k
ksubsets :: (Eq a, Typeable a) => Int -> [a] -> Gen [a]
ksubsets k as = getSet <$> fromSpecies (S.ksubset (fromIntegral k)) as

-- | Generate unique pairs
pairs :: (Eq a, Typeable a) => [a] -> Gen (a, a)
pairs as = fromPairSpeciesStructure <$> fromSpecies pairsSpecies as

-- | Generate trees from a label set
trees ::  (Eq a, Typeable a) => [a] -> Gen (Tree a)
trees as = fromTreeSpeciesStructure <$> fromSpecies treesSpecies as

-- | Generate ordered pairs
--   We do not enforce that there is an Ord a, the pairs
--   are ordered by virtue of the ksubset combinator in the order of
--   apparition of the elements in the list of labels
orderedPairs :: (Eq a, Typeable a) => [a] -> Gen (a, a)
orderedPairs as = fromOrderedPairSpeciesStructure <$> fromSpecies orderedPairsSpecies as

-- | Generate elements from a species, given a set of labels
--   using reservoir sampling to select a structure randomly
--   this returns a sized generator which will draw elements from a larger
--   set of generated structures as the size grows
fromSpecies :: (S.Enumerable f, Eq a, Typeable a) => S.SpeciesAST -> [a] -> Gen (f a)
fromSpecies s ls = sized $ \n ->
  head <$> reservoirSample 1 (Prelude.take (n + 1) $ S.enumerate s ls)

----------
-- SPECIES
----------

-- | Species of pairs of distinct elements
pairsSpecies :: S.Species s => s
pairsSpecies = S.x * S.x * S.set

-- | Species of (rose) trees
treesSpecies :: S.Species s => s
treesSpecies = S.x * forestsSpecies

-- | Species of forests
forestsSpecies :: S.Species s => s
forestsSpecies = S.linOrds `S.o` treesSpecies

-- | get elements as a pair from the pair species
fromPairSpeciesStructure :: ((Id :*: Id) :*: Set) a -> (a, a)
fromPairSpeciesStructure ((Id a :*: Id b) :*: _) = (a, b)

orderedPairsSpecies :: S.Species s => s
orderedPairsSpecies = S.ksubset 2

-- | get elements as a pair from the ordered pair species
fromOrderedPairSpeciesStructure :: Set a -> (a, a)
fromOrderedPairSpeciesStructure s =
  case getSet s of
    (a : b : _) -> (a, b)
    _ -> Prelude.error "this case should not happen"

-- | recover a tree from a species element for trees
fromTreeSpeciesStructure :: ((Id :*: Id) :*: Set) a -> Tree a
fromTreeSpeciesStructure ((Id a :*: Id _) :*: _) = Node a []

---------
-- RANDOM
---------

-- | reservoir sampling from a list of elements
reservoirSample :: (S.Enumerable f, Eq a, Typeable a) => Int -> [f a] -> Gen [f a]
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
