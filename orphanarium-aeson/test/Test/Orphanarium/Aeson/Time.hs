{-# LANGUAGE TemplateHaskell #-}
module Test.Orphanarium.Aeson.Time where

import           Orphanarium.Aeson
import           Orphanarium.Aeson.Time

import           Test.QuickCheck


prop_genTime :: Property
prop_genTime = forAll genTime jsonProp


return []
tests :: IO Bool
tests = $quickCheckAll
