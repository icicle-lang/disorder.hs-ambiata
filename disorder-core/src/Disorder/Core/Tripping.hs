{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Disorder.Core.Tripping where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck


tripping :: (Applicative f, Show (f a), Eq (f a)) => (a -> b) -> (b -> f a) -> a -> Property
tripping to fro a =
  (fro . to) a === pure a

trippingWith :: (Applicative f, Show (f a), Eq (f a), Eq (f Bool)) => (a -> a -> Bool) -> (a -> b) -> (b -> f a) -> a -> Property
trippingWith eq to fro a =
  let tripped = (fro . to) a
      purea   = pure a
  in counterexample (show tripped <> " /= " <> show purea)
     (property ((eq <$> tripped <*> purea) == pure True))
