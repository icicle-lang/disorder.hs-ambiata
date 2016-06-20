{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Jack.Shrink (
    shrinkIntegral

  , sequenceShrink
  , sequenceShrinkOne
  , sequenceShrinkList

  , shrinkOne
  , shrinkList

  , halves
  , removes
  ) where

import           Data.Ord (Ord(..))
import           Data.Functor (fmap)
import           Data.Function (($), (.))
import           Data.Monoid ((<>))
import qualified Data.List as List
import           Data.Int (Int)

import           Disorder.Jack.Tree

import           Prelude (Num(..), Integral, div)

import qualified Test.QuickCheck.Arbitrary as QC


-- | Shrink an integral, but don't allow it to go below the specified minimum.
shrinkIntegral :: Integral a => a -> a -> [a]
shrinkIntegral x_min x =
  fmap (+ x_min) $ QC.shrinkIntegral (x - x_min)

-- | Turn a list of trees in to a tree of lists, opting to shrink only the
--   elements of the list (i.e. the size of the list will always be the same).
--
sequenceShrinkOne :: [Tree a] -> Tree [a]
sequenceShrinkOne =
  sequenceShrink (\xs -> shrinkOne shrinks xs)

-- | Turn a list of trees in to a tree of lists, opting to shrink both the list
--   itself and the elements in the list during traversal.
--
sequenceShrinkList :: [Tree a] -> Tree [a]
sequenceShrinkList =
  sequenceShrink (\xs -> shrinkList xs <> shrinkOne shrinks xs)

-- | Turn a list of trees in to a tree of lists, using the supplied function to
--   merge shrinking options.
--
sequenceShrink :: ([Tree a] -> [[Tree a]]) -> [Tree a] -> Tree [a]
sequenceShrink merge xs =
  Node
    (fmap outcome xs)
    (fmap (sequenceShrink merge) $ merge xs)

-- | Shrink each of the elements in input list using the supplied shrinking
--   function.
--
shrinkOne :: (a -> [a]) -> [a] -> [[a]]
shrinkOne shr = \case
  [] ->
    []
  x0 : xs0 ->
    [ x1 : xs0 | x1 <- shr x0 ] <>
    [ x0 : xs1 | xs1 <- shrinkOne shr xs0 ]

-- | Produce a smaller permutation of the input list.
--
shrinkList :: [a] -> [[a]]
shrinkList xs = do
 List.concatMap
   (\k -> removes k xs)
   (halves $ List.length xs)

-- | Produces a list containing the results of halving a number over and over
--   again.
--
--   > halves 30 == [30,15,7,3,1]
--   > halves 128 == [128,64,32,16,8,4,2,1]
--
halves :: Int -> [Int]
halves =
  List.takeWhile (> 0) .
  List.iterate (`div` 2)

-- | Permutes a list by removing 'k' consecutive elements from it:
--
--   > removes 2 [1,2,3,4,5,6] == [[3,4,5,6],[1,2,5,6],[1,2,3,4]]
--
removes :: Int -> [a] -> [[a]]
removes k0 xs0 =
  let
    loop k n xs =
      let
        hd = List.take k xs
        tl = List.drop k xs
      in
        if k > n then
          []
        else if List.null tl then
          [[]]
        else
          tl : fmap (hd <>) (loop k (n - k) tl)
  in
    loop k0 (List.length xs0) xs0
