{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Core.Tripping where

import           Disorder.Core

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_tripping =
  tripping id (Just :: Int -> Maybe Int)

prop_tripping_neg =
  neg . property $ tripping id (const Nothing :: Int -> Maybe Int)

return []
tests = $quickCheckAll
