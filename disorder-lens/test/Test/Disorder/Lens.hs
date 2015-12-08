{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Lens where

import           Disorder.Lens

import           Control.Lens ( _Just, Prism'  )
import           Data.Traversable

import           Test.QuickCheck

import           Prelude

prop_prismSymmetry :: (Arbitrary a, Show a, Eq a) => a -> Property
prop_prismSymmetry = prismSymmetry _Just

prop_ConverseSymmetry :: (Arbitrary a, Show a, Eq a) => Maybe a -> Property
prop_ConverseSymmetry = prismConverseSymmetry _Just

prop_prismLaws :: Property
prop_prismLaws = prismLaws (_Just :: Prism' (Maybe Int) Int)

prop_traversalPure :: (Arbitrary a, Show a, Eq a) => [a] -> Property
prop_traversalPure = traversalPure traverse

return []
tests :: IO Bool
tests = $quickCheckAll
