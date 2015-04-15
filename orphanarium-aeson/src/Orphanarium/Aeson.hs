{-# LANGUAGE OverloadedStrings #-}
module Orphanarium.Aeson where

import           Control.Applicative ( pure )
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as M

import           Test.QuickCheck

jsonProp :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> Property
jsonProp a =
  parseEither (parseJSON . toJSON) a === Right a

jsonVersionProp :: (ToJSON a, Eq a, Show a) => Value -> a -> Property
jsonVersionProp v j = case toJSON j of
    Object o    -> M.lookup "version" o === pure v
    _           -> counterexample "Json value should be an object." False
