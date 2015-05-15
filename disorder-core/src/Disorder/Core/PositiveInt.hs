module Disorder.Core.PositiveInt (
    PositiveInt (..)
  ) where

import           Control.Applicative

import           Test.QuickCheck


data PositiveInt = PositiveInt {
    value :: Int
  } deriving (Eq, Show)

instance Arbitrary PositiveInt where
  arbitrary = PositiveInt <$> choose (1, maxBound)
