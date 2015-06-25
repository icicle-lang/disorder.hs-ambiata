{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Core.Gen where

import           Disorder.Core.Gen
import           Disorder.Core.IO
import           Disorder.Core.OrdPair

import           Test.QuickCheck
import           Data.Maybe
import           Data.List (partition)


prop_vectorOfSize :: OrdPair (Positive Int) -> Positive Int -> Property
prop_vectorOfSize (OrdPair (Positive x) (Positive y)) (Positive s) = testIO $ do
  l <- generate . resize s $ vectorOfSize x y (arbitrary :: Gen Int)
  return $ length l >= x .&. length l <= y

prop_chooseSize :: OrdPair (Positive Int) -> Positive Int -> Property
prop_chooseSize (OrdPair (Positive x) (Positive y)) (Positive s) = testIO $ do
  s' <- generate . resize s $ chooseSize x y
  return $ s' >= x .&. s' <= y

prop_maybeGen :: Property
prop_maybeGen = testIO $ do
  ma <- generate $ vectorOf 10000 (maybeGen (arbitrary :: Gen Int))
  -- not the best statistical test but we want to make sure that
  -- we have "enough" Nothings in the list
  return $
    let (justs, nothings) = partition isJust ma
    in  (length justs >= 8000) .&.
        (length nothings >= 100)

return []
tests :: IO Bool
tests = $quickCheckAll
