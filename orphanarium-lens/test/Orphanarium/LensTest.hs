{-# LANGUAGE TemplateHaskell #-}
module Orphanarium.LensTest where

import           Orphanarium.Lens

import           Control.Lens ( _Just  )
import           Data.Traversable ( traverse )

import           Test.QuickCheck

prop_prismSymmetry :: (Arbitrary a, Show a, Eq a) => a -> Property
prop_prismSymmetry = prismSymmetry _Just

prop_ConverseSymmetry :: (Arbitrary a, Show a, Eq a) => Maybe a -> Property
prop_ConverseSymmetry = prismConverseSymmetry _Just

prop_prismLaws :: (Arbitrary a, Show a, Eq a) => Maybe a -> a -> Property
prop_prismLaws = prismLaws _Just

prop_traversalPure :: (Arbitrary a, Show a, Eq a) => [a] -> Property
prop_traversalPure = traversalPure traverse

return []
tests :: IO Bool
tests = $quickCheckAll
