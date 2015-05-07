{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Core.Property where

import           Data.Text (Text)

import           Disorder.Core

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_failWith :: Text -> Property
prop_failWith t =
  neg (failWith t)

prop_notEquals :: (Arbitrary a, Show a, Eq a) => a -> a -> Property
prop_notEquals x y =
  (x /= y) ==> x =/= y

prop_equalsXor :: (Arbitrary a, Show a, Eq a) => a -> a -> Property
prop_equalsXor x y =
  (x === y) .^. (x =/= y)

prop_negEquals :: (Arbitrary a, Show a, Eq a) => a -> a -> Property
prop_negEquals x y =
  (x =/= y) <=> neg (x === y)

-- |
-- @p .^. neg p@
prop_negXor :: (Arbitrary a, Show a, Eq a) => a -> a -> Property
prop_negXor x y =
  (x === y) .^. neg (x === y)

return []
tests :: IO Bool
tests = $quickCheckAll
