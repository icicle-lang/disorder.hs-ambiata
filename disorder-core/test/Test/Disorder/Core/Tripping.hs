{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Core.Tripping where

import           Data.Function
import           Disorder.Core

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_tripping =
  tripping id (Just :: Int -> Maybe Int)

prop_tripping_neg =
  neg . property $ tripping id (const Nothing :: Int -> Maybe Int)

prop_tripping_on =
  trippingOn (+1) id (Just :: Int -> Maybe Int)

prop_tripping_on_neg =
  neg . property $ trippingOn (+1) id (const Nothing :: Int -> Maybe Int)

prop_tripping_with =
  trippingWith ((===) `on` fmap (+1)) id (Just :: Int -> Maybe Int)

prop_tripping_with_neg =
  neg . property $ trippingWith ((===) `on` fmap (+1)) id (const Nothing :: Int -> Maybe Int)

return []
tests = $quickCheckAll
