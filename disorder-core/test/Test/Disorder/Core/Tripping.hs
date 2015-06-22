{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Core.Tripping where

import           Disorder.Core

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_tripping =
  tripping id (Just :: Int -> Maybe Int)

prop_tripping_neg =
  neg . property $ tripping id (const Nothing :: Int -> Maybe Int)

prop_tripping_with =
  trippingWith (==) id (Just :: Int -> Maybe Int)

prop_tripping_with_neg =
  neg . property $ trippingWith (==) id (const Nothing :: Int -> Maybe Int)

return []
tests = $quickCheckAll
