{-# LANGUAGE TemplateHaskell #-}
module Test.Orphanarium.Core.Property where

import           Orphanarium.Core

import           Test.QuickCheck

prop_notEquals :: (Arbitrary a, Show a, Eq a) => a -> a -> Property
prop_notEquals x y = (x /= y) ==> x =/= y

prop_equalsXor :: (Arbitrary a, Show a, Eq a) => a -> a -> Property
prop_equalsXor x y = (x === y) .^. (x =/= y)

prop_negEquals :: (Arbitrary a, Show a, Eq a) => a -> a -> Property
prop_negEquals x y = (x =/= y) <=> neg (x === y)

-- An arbitrary for properties?

-- |
-- @p .^. neg p@
prop_negXor :: (Arbitrary a, Show a, Eq a) => a -> a -> Property
prop_negXor x y = (x === y) .^. neg (x === y)

return []
tests :: IO Bool
tests = $quickCheckAll
