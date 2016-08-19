{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Core.Gen where

import           Data.List (nub)

import           Disorder.Core.Gen
import           Disorder.Core.IO
import           Disorder.Core.OrdPair
import           Disorder.Core.Run

import           Test.QuickCheck
import           Data.Maybe
import           Data.List (partition)


prop_vectorOfSize :: OrdPair (Positive Int) -> Positive Int -> Property
prop_vectorOfSize (OrdPair (Positive x) (Positive y)) (Positive s) = testIO $ do
  l <- generate . resize s $ vectorOfSize x y (arbitrary :: Gen Int)
  return $ length l >= x .&&. length l <= y

prop_chooseSize :: OrdPair (Positive Int) -> Positive Int -> Property
prop_chooseSize (OrdPair (Positive x) (Positive y)) (Positive s) = testIO $ do
  s' <- generate . resize s $ chooseSize x y
  return $ s' >= x .&&. s' <= y

prop_maybeGen :: Property
prop_maybeGen = testIO $ do
  ma <- generate $ vectorOf 10000 (maybeGen (arbitrary :: Gen Int))
  -- not the best statistical test but we want to make sure that
  -- we have "enough" Nothings in the list
  return $
    let (justs, nothings) = partition isJust ma
    in  (length justs >= 8000) .&.
        (length nothings >= 100)

prop_genFromMaybe :: Property
prop_genFromMaybe =
  testIO $ do
    ma <- generate $ genFromMaybe (arbitrary :: Gen (Maybe ()))
    return $ ma == ()

prop_vectorOfUnique :: Property
prop_vectorOfUnique =
  forAll (choose (0, 100)) $ \n ->
    forAll (vectorOfUnique n genValidUtf8) $ \xs ->
      (xs, length xs) === (nub xs, n)

prop_vectorOfUnique' :: Property
prop_vectorOfUnique' =
  expectFailure $
    forAll (vectorOfUnique' 0 10 genValidUtf8) $ \xs ->
      xs === nub xs

prop_listOf1Unique :: Property
prop_listOf1Unique =
  forAll (listOf1Unique genValidUtf8) $ \xs ->
    conjoin [
        xs === nub xs
      , (length xs >= 1) === True
      ]

return []
tests :: IO Bool
tests = $disorderCheckAll
