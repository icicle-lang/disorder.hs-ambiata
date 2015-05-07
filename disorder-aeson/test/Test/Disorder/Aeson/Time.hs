{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Aeson.Time where

import           Disorder.Aeson
import           Disorder.Aeson.Time

import           Test.QuickCheck


prop_genTime :: Property
prop_genTime = forAll genTime jsonProp


return []
tests :: IO Bool
tests = $quickCheckAll
