{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Core.OrdPair where

import           Disorder.Core.OrdPair
import           Disorder.Core.Run

import           Test.QuickCheck


prop_uniquePair :: (Arbitrary a, Show a, Eq a, Ord a) => OrdPair a -> Bool
prop_uniquePair (OrdPair a b) = a <= b


return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
