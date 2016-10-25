{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Jack.Tripping (
    tripping
  , trippingRender
  , trippingString
  ) where

import           Disorder.Jack.Property
import           Disorder.Jack.Property.Diff

import           Control.Applicative (Applicative(..))

import           Data.Eq (Eq(..))
import           Data.Function (($), (.))
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Show (Show(..))
import           Text.Show.Pretty (ppShow, parseValue)


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

    diff = do
      o <- parseValue $ render original
      r <- parseValue $ render roundtrip
      pure [
          "=== - Original / + Roundtrip ==="
        , renderDiffs o r
        ]

    comparison = [
        "=== Original ==="
      , render original
      , ""
      , "=== Roundtrip ==="
      , render roundtrip
      ]
  in
    counterexample "" .
    counterexample "Roundtrip failed." .
    counterexample "" .
    counterexample (List.intercalate "\n" $ fromMaybe comparison diff) $
      property (roundtrip == original)
