module Disorder.Core.OrdPair (
    OrdPair(..)
  , toTupleOrd
  ) where

import           Control.Applicative

import           Test.QuickCheck


data OrdPair a =
  OrdPair a a
  deriving (Eq, Show)

instance (Arbitrary a, Ord a) => Arbitrary (OrdPair a) where
  arbitrary =
    (\a b -> if a < b then OrdPair a b else OrdPair b a)
    <$> arbitrary
    <*> arbitrary

toTupleOrd :: OrdPair a -> (a, a)
toTupleOrd (OrdPair a b) = (a, b)
