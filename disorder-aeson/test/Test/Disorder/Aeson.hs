{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Disorder.Aeson where

import           Disorder.Aeson

import           Test.QuickCheck

prop_jsonProp :: Int -> Property
prop_jsonProp =
  jsonProp

return []
tests :: IO Bool
tests = $quickCheckAll
