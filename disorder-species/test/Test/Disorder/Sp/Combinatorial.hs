{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Sp.Combinatorial where

import           Control.Applicative
import           Disorder.Sp.Combinatorial

import           Test.QuickCheck
import           Test.Disorder.Sp.Arbitraries

prop_kintPartitions (PositiveIntegerSmall n) = forAll (choose (1, n)) $ \k ->
  conjoin [
    all (== n) (sum <$> kintPartitions n k)
  ]

--

return []
tests :: IO Bool
tests = $quickCheckAll
