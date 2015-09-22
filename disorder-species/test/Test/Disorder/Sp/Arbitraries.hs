{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Disorder.Sp.Arbitraries (
  PositiveSmall (..)
, PositiveIntegerSmall (..)
, simpsons
) where

import           Control.Applicative

import qualified Data.Text as T

import           Test.QuickCheck

data PositiveSmall =
  PositiveSmall Int
  deriving (Eq, Show)

instance Arbitrary PositiveSmall where
  arbitrary = PositiveSmall <$> choose (1, 5)

data PositiveIntegerSmall =
  PositiveIntegerSmall Integer
  deriving (Eq, Show, Ord)

instance Arbitrary PositiveIntegerSmall where
  arbitrary = PositiveIntegerSmall <$> choose (1, 5)

----------
-- HELPERS
----------

simpsons :: [T.Text]
simpsons =
  [
    "homer"
  , "marge"
  , "maggie"
  , "lisa"
  , "bart"
  , "flanders"
  , "moe"
  , "barney"
  ]
