{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Core.Property where

import           Control.Applicative       ((<$>))
import           Data.AEq                  (AEq)
import qualified Data.AEq                  as AEQ
import           Data.List                 (delete)
import           Data.Text                 (Text)

import           Disorder.Core

import           Numeric.IEEE

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

prop_approxNotEquals :: (AEq a, Arbitrary a, Show a) => a -> a -> Property
prop_approxNotEquals x y =
  (not (x AEQ.~== y)) ==> neg (x ~~~ y)

prop_idApproxEquals :: (AEq a, Arbitrary a, Show a) => a -> Property
prop_idApproxEquals x = x ~~~ x

prop_floatApproxEquals :: Double -> Property
prop_floatApproxEquals x =
  ((abs x) > 1.0) ==> (x + epsilon) ~~~ x

prop_floatApproxNotEquals :: Double -> Int -> Property
prop_floatApproxNotEquals x n =
  n /= 0 ==> neg ((x + (fromIntegral n)) ~~~ x)

prop_negEquals :: (Arbitrary a, Show a, Eq a) => a -> a -> Property
prop_negEquals x y =
  (x =/= y) <=> neg (x === y)

-- |
-- @p .^. neg p@
prop_negXor :: (Arbitrary a, Show a, Eq a) => a -> a -> Property
prop_negXor x y =
  (x === y) .^. neg (x === y)

prop_areEquivalent :: (Eq a, Show a) => [a] -> Property
prop_areEquivalent ls =
  forAll (shuffle ls) $ areEquivalent ls
  where
    shuffle [] = return []
    shuffle xs = do
      x <- elements xs
      (x:) <$> shuffle (delete x xs)

prop_areNotEquivalent :: (Eq a, Show a) => [a] -> [a] -> Property
prop_areNotEquivalent ls rs =
 not (all (`elem`ls) rs && all (`elem`rs) ls) ==>
   expectFailure $ areEquivalent ls rs


return []
tests :: IO Bool
tests = $quickCheckAll
