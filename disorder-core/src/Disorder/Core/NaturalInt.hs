module Disorder.Core.NaturalInt (
    NaturalInt (..)
  ) where

import           Control.Applicative

import           Test.QuickCheck


data NaturalInt = NaturalInt {
    value :: Int
  } deriving (Eq, Show)

instance Arbitrary NaturalInt where
  arbitrary = NaturalInt <$> choose (0, maxBound)
