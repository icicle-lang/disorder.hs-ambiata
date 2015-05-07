module Disorder.Aeson where

import           Data.Aeson
import           Data.Aeson.Types

import           Test.QuickCheck

jsonProp :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> Property
jsonProp a =
  parseEither (parseJSON . toJSON) a === Right a
