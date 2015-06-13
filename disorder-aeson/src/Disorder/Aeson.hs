module Disorder.Aeson where

import           Disorder.Core

import           Data.Aeson
import           Data.Aeson.Types

import           Test.QuickCheck

jsonProp :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> Property
jsonProp =
  tripping toJSON (parseEither parseJSON)
