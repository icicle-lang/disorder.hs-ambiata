{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Core.UniquePair where

import           Disorder.Core.Property
import           Disorder.Core.UniquePair
import           Disorder.Core.Run

import           Test.QuickCheck


prop_uniquePair :: (Arbitrary a, Show a, Eq a) => UniquePair a -> Property
prop_uniquePair (UniquePair a b) = a =/= b


return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFast
