{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Core.PositiveInt where

import           Disorder.Core.PositiveInt

import           Test.QuickCheck

prop_is_positive (PositiveInt i) = i > 0

return []
tests :: IO Bool
tests = $quickCheckAll

