{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Orphanarium.AesonTest where

import           Orphanarium.Aeson

import           Test.QuickCheck

prop_jsonProp :: Int -> Property
prop_jsonProp =
  jsonProp

return []
tests :: IO Bool
tests = $quickCheckAll
