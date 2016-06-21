{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Jack.Shrink where

import           Control.Monad (Monad(..))

import           Data.Bool (Bool(..), (&&))
import           Data.Eq (Eq(..))
import           Data.Function (($), (.))
import qualified Data.List as List
import           Data.Ord (Ord(..), min, max)

import           Disorder.Jack.Combinators
import           Disorder.Jack.Property
import           Disorder.Jack.Shrink

import           System.IO (IO)


prop_shrinkTowards_unique :: Property
prop_shrinkTowards_unique =
  gamble boundedInt $ \x ->
  gamble boundedInt $ \y ->
    let
      ss = shrinkTowards x y
    in
      List.nub ss == ss

prop_shrinkTowards_range :: Property
prop_shrinkTowards_range =
  gamble boundedInt $ \x ->
  gamble boundedInt $ \y ->
    let
      s_min = min x y
      s_max = max x y
      valid s =
        s >= s_min && s <= s_max
    in
      List.all valid $ shrinkTowards x y

return []
tests :: IO Bool
tests =
  $forAllProperties . quickCheckWithResult $ stdArgs { maxSuccess = 100 }
