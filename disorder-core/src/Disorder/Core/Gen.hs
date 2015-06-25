module Disorder.Core.Gen (
    vectorOfSize
  , chooseSize
  , maybeGen
  ) where

import           Test.QuickCheck.Gen
import           Control.Applicative


-- | Return a vector whose size is within the provided bounds
vectorOfSize :: Int -> Int -> Gen a -> Gen [a]
vectorOfSize min' max' gen =
  chooseSize min' max' >>= flip vectorOf gen

-- | Return an 'Int' which is between the provided range, and influenced by the current "size"
chooseSize :: Int -> Int -> Gen Int
chooseSize min' max' =
  sized (return . min max' . max min')

-- | from a generator return a generator that will generate Nothing
--   half the time and Just half the time
maybeGen :: Gen a -> Gen (Maybe a)
maybeGen g = do
  b <- elements [True, False]
  if b then Just <$> g else elements [Nothing]
