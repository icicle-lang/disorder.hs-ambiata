{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Orphanarium.Aeson where

import           Orphanarium.Aeson

import           Test.QuickCheck

prop_jsonProp :: Int -> Property
prop_jsonProp =
  jsonProp

return []
tests :: IO Bool
tests = $quickCheckAll
