module Disorder.Core.UniquePair (
    UniquePair(..)
  , toTuple
  ) where

import           Control.Applicative

import           Test.QuickCheck


data UniquePair a =
  UniquePair {
      fst :: a
    , snd :: a
    } deriving (Eq, Show)

instance (Arbitrary a, Eq a) => Arbitrary (UniquePair a) where
  arbitrary = suchThat (UniquePair <$> arbitrary <*> arbitrary) (uncurry (/=) . toTuple)

toTuple :: UniquePair a -> (a, a)
toTuple (UniquePair a b) = (a, b)
