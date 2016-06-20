{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Jack.Tree (
    Tree(..)
  , foldTree
  , foldForest
  , unfoldTree
  , unfoldForest
  , filterTree
  , filterForest
  , expandTree
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Comonad (Comonad(..), ComonadApply(..))
import           Control.DeepSeq (NFData(..))
import           Control.Monad (Monad(..))

import           Data.Bool (Bool)
import           Data.Data (Data)
import           Data.Eq (Eq)
import           Data.Foldable (Foldable(..))
import           Data.Function (($), (.), id)
import           Data.Functor (Functor(..), (<$>))
import qualified Data.List as List
import           Data.Monoid ((<>))
import           Data.Ord (Ord)
import           Data.Traversable (Traversable(..))
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           Prelude (seq)

import           Text.Show (Show)


-- | A rose tree which represents a random generated outcome, and all the ways
--   in which it can be made smaller.
--
--   This tree is exactly the same as 'Data.Tree' in every way except that
--   Applicative '<*>' and Monad '>>=' walk the tree in the reverse order. This
--   modification is critical for shrinking to reach a minimal counterexample.
--
data Tree a =
  Node {
    -- | The generated outcome.
    outcome :: !a

    -- | All the possible shrinks of this outcome. This should be ordered
    --   smallest to largest as if property still fails with the first shrink in
    --   the list then we will commit to that path and none of the others will
    --   be tried (i.e. there is no backtracking).
  , shrinks :: [Tree a]
  } deriving (Eq, Ord, Show, Generic, Data, Typeable)

instance Functor Tree where
  fmap f (Node x xs) =
    Node (f x) $ fmap (fmap f) xs

instance Applicative Tree where
  pure x =
    Node x []

  (<*>) (Node f fs) x@(Node y ys) =
      Node (f y) $
      -- Data.Tree would have `fmap (f <$>) ys <> fmap (<*> x) fs`
        fmap (<*> x) fs <>
        fmap (f <$>) ys

instance Monad Tree where
  return =
    pure

  (>>=) (Node x xs) k =
    let
      Node y ys = k x
    in
      Node y $
      -- Data.Tree would have `ys <> fmap (>>= k) xs`
        fmap (>>= k) xs <> ys

instance Traversable Tree where
  traverse f (Node x xs) =
    Node <$> f x <*> traverse (traverse f) xs

instance Foldable Tree where
  foldMap f (Node x xs) =
    f x <> foldMap (foldMap f) xs

instance Comonad Tree where
  extract (Node x _) =
    x

  duplicate x@(Node _ ys) =
    Node x (fmap duplicate ys)

instance ComonadApply Tree where
  (<@>) =
    (<*>)

  (<@) =
    (<*)

  (@>) =
    (*>)

instance NFData a => NFData (Tree a) where
  rnf (Node x xs) =
    rnf x `seq` rnf xs

-- | Fold over a 'Tree'.
foldTree :: (a -> x -> b) -> ([b] -> x) -> Tree a -> b
foldTree f g (Node x xs) =
  f x (foldForest f g xs)

-- | Fold over a list of trees.
foldForest :: (a -> x -> b) -> ([b] -> x) -> [Tree a] -> x
foldForest f g =
  g . fmap (foldTree f g)

-- | Build a 'Tree' from an unfolding function and a seed value.
unfoldTree :: (b -> a) -> (b -> [b]) -> b -> Tree a
unfoldTree f g x =
  Node (f x) (unfoldForest f g x)

-- | Build a list of trees from an unfolding function and a seed value.
unfoldForest :: (b -> a) -> (b -> [b]) -> b -> [Tree a]
unfoldForest f g =
  fmap (unfoldTree f g) . g

-- | Apply an additional unfolding function to an existing tree.
--
--   The root outcome remains intact, only the shrinks are affected, this
--   applies recursively, so shrinks can only ever be added using this
--   function.
--
--   If you want to replace the shrinks altogether, try:
--
--   > unfoldTree f (outcome oldTree)
--
expandTree :: (a -> [a]) -> Tree a -> Tree a
expandTree f (Node x xs) =
  --
  -- Ideally we could put the 'unfoldForest' nodes before the 'fmap expandTree'
  -- nodes, so that we're culling from the top down and we would be able to
  -- terminate our search faster, but this prevents minimal shrinking.
  --
  -- We'd need some kind of tree transpose to do this properly.
  --
  Node x (fmap (expandTree f) xs <> unfoldForest id f x)

-- | Recursively discard any shrinks whose outcome does not pass the predicate.
--   /Note that the root outcome can never be discarded./
filterTree :: (a -> Bool) -> Tree a -> Tree a
filterTree f (Node x xs) =
  Node x (filterForest f xs)

-- | Recursively discard any trees whose outcome does not pass the predicate.
filterForest :: (a -> Bool) -> [Tree a] -> [Tree a]
filterForest f =
  fmap (filterTree f) . List.filter (f . outcome)
