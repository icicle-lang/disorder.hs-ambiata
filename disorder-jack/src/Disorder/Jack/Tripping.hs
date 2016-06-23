{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Jack.Tripping (
    tripping
  , trippingRender
  , trippingString
  ) where

import           Disorder.Jack.Property

import           Control.Applicative (Applicative(..))

import           Data.Eq (Eq(..))
import           Data.Function (($), (.))
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Show (Show(..))
import           Text.Show.Pretty (ppShow)


tripping :: (Applicative f, Eq (f a), Show (f a)) => (a -> b) -> (b -> f a) -> a -> Property
tripping =
  trippingString ppShow

trippingRender :: (Applicative f, Eq (f a)) => (f a -> Text) -> (a -> b) -> (b -> f a) -> a -> Property
trippingRender render =
  trippingString (T.unpack . render)

trippingString :: (Applicative f, Eq (f a)) => (f a -> String) -> (a -> b) -> (b -> f a) -> a -> Property
trippingString render to fro x =
  let
    roundtrip =
      (fro . to) x

    original =
      pure x
  in
    counterexample "" .
    counterexample "Roundtrip failed." .
    counterexample "" .
    counterexample "=== Original ===" .
    counterexample (render original) .
    counterexample "" .
    counterexample "=== Roundtrip ===" .
    counterexample (render roundtrip) $
      property (roundtrip == original)
