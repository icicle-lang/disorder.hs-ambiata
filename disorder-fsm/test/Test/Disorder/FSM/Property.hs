{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.FSM.Property where

import           Control.Arrow (first)

import           Data.List (sort)

import           Disorder.FSM.Property (frequencyShuffle)

import           Prelude

import           Test.QuickCheck


prop_frequencyShuffle_all :: [(Positive Int, Char)] -> Property
prop_frequencyShuffle_all kxs' =
  let kxs = fmap (first getPositive) kxs'
  in forAll (frequencyShuffle kxs) $ \xs ->
    sort xs === sort (fmap snd kxs)


return []
tests :: IO Bool
tests = $quickCheckAll
