module Disorder.Core.Tripping where

import           Control.Applicative

import           Test.QuickCheck

-- | Generalized round-trip property function
tripping :: (Applicative f, Show (f a), Eq (f a)) => (a -> b) -> (b -> f a) -> a -> Property
tripping to fro a =
  (fro . to) a === pure a
