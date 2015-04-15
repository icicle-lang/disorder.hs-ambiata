{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Orphanarium.AesonTest where

import           Orphanarium.Aeson

import           Data.Aeson ( (.=), object, toJSON )
import qualified Data.Text as T

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_jsonProp :: Int -> Property
prop_jsonProp =
  jsonProp

prop_jsonVersionProp :: T.Text -> Property
prop_jsonVersionProp t = jsonVersionProp (toJSON t) $ object ["version" .= t]

return []
tests :: IO Bool
tests = $quickCheckAll
