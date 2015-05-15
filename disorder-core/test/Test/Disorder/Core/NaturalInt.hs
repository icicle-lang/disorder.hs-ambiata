{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Core.NaturalInt where

import           Disorder.Core.NaturalInt

import           Test.QuickCheck


prop_isnatural (NaturalInt i) = i >= 0

return []
tests :: IO Bool
tests = $quickCheckAll

