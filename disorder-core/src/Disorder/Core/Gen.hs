module Disorder.Core.Gen (
    vectorOfSize
  , chooseSize
  , maybeGen
  -- * re-exports from quickcheck-text
  , genValidUtf8
  , genValidUtf81
  , utf8BS
  , utf8BS1
  ) where

import           Control.Applicative

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Utf8

-- | Return a vector whose size is within the provided bounds
vectorOfSize :: Int -> Int -> Gen a -> Gen [a]
vectorOfSize min' max' gen =
  chooseSize min' max' >>= flip vectorOf gen

-- | Return an 'Int' which is between the provided range, and influenced by the current "size"
chooseSize :: Int -> Int -> Gen Int
chooseSize min' max' =
  sized (return . min max' . max min')

-- | from a generator return a generator that will generate Nothing sometimes
maybeGen :: Gen a -> Gen (Maybe a)
maybeGen g = sized $ \s ->
  frequency [
    (1, return Nothing),
    (s, Just <$> resize (s `div` 2) g)]
