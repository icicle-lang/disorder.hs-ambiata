{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Core.Property where

import           Data.AEq                  (AEq)
import qualified Data.AEq                  as AEQ
import           Data.Functor
import           Data.List                 (delete)
import           Data.Text                 (Text)

import           Disorder.Core

import           Numeric.IEEE

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Prelude

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
  forAll (shuf ls) $ \rs ->
    ls =\\= rs
  where
    shuf [] = return []
    shuf xs = do
      x <- elements xs
      (x:) <$> shuf (delete x xs)

prop_areNotEquivalent :: (Eq a, Show a) => [a] -> [a] -> Property
prop_areNotEquivalent ls rs =
 not (all (`elem`ls) rs && all (`elem`rs) ls) ==>
   expectFailure $ ls =\\= rs

prop_realEq_pos :: Int -> Property
prop_realEq_pos x =
  x' ### x'
  where
    x' :: Double
    x' = fromIntegral x

prop_realEq_neg :: Double -> Property
prop_realEq_neg n = forAll (elements [0 / 0, n / 0] :: Gen Double) $ \bad ->
  expectFailure $ bad ### bad

prop_realEq_neq :: UniquePair Int -> Property
prop_realEq_neq (UniquePair n m) =
  expectFailure $ n' ### m'
  where
    n' :: Double
    n' = fromIntegral n

    m' :: Double
    m' = fromIntegral m

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
