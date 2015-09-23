{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Sp.Combinatorial where

import           Disorder.Sp.Combinatorial

import           Test.QuickCheck
import           Test.Disorder.Sp.Arbitraries

prop_kintPartitions (PositiveIntegerSmall n) = forAll (choose (1, n)) $ \k ->
  let kints = kintPartitions n k in
  conjoin [
    all ((== n) . sum) kints
  , all ((== fromInteger k) . length) kints
  ]

--

return []
tests :: IO Bool
tests = $quickCheckAll
