{-# LANGUAGE TemplateHaskell #-}
module Test.Orphanarium.Core.UniquePair where

import           Orphanarium.Core.Property
import           Orphanarium.Core.UniquePair

import           Test.QuickCheck


prop_uniquePair :: (Arbitrary a, Show a, Eq a) => UniquePair a -> Property
prop_uniquePair (UniquePair a b) = a =/= b


return []
tests :: IO Bool
tests = $quickCheckAll
