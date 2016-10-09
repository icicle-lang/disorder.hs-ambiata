{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Jack.Combinators where

import           Control.Applicative (Applicative(..))
import           Control.Comonad (duplicate)
import           Control.Monad (Monad(..))

import           Data.Bool (Bool(..), (&&))
import           Data.Char (Char)
import           Data.Eq (Eq(..))
import qualified Data.Foldable as Foldable
import           Data.Function (($), (.), flip)
import           Data.Int (Int)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (Ord(..))
import qualified Data.Set as Set

import           Disorder.Jack.Combinators
import           Disorder.Jack.Core
import           Disorder.Jack.Property
import           Disorder.Jack.Tree

import           Prelude (even)

import           System.IO (IO)


prop_noShrink :: Property
prop_noShrink =
  gamble (mapTree duplicate $ noShrink boundedInt) $ \(Node _ xs) ->
    List.null xs

prop_choose :: Property
prop_choose =
  gamble boundedInt $ \n ->
  gamble boundedInt $ \m ->
  gamble (mapTree duplicate $ chooseInt (n, m)) $ \xs ->
    let
      x_min =
        min n m

      x_max =
        max n m

      valid x =
        x >= x_min && x <= x_max
    in
      -- it takes an enormous amount of time to expore the entire shrink space,
      -- so we just check the first 1000 in a depth first search of the tree
      List.all valid . List.take 1000 $ Foldable.toList xs

prop_oneof :: Property
prop_oneof =
  gamble (mapTree duplicate $ oneOf [pure 'A', pure 'B', pure 'C']) isABC

prop_elements :: Property
prop_elements =
  gamble (mapTree duplicate $ elements ['A', 'B', 'C']) isABC

prop_frequency :: Property
prop_frequency =
  gamble (mapTree duplicate $ frequency [(1, pure 'A'), (1, pure 'B'), (1, pure 'C')]) isABC

isABC :: Tree Char -> Bool
isABC (Node x xs) =
  case x of
    'A' ->
      List.null xs
    'B' ->
      xs == [
          Node 'A' []
        ]
    _ ->
      xs == [
          Node 'A' []
        , Node 'B' [Node 'A' []]
        ]

prop_sublistOf :: Property
prop_sublistOf =
  let
    xs = Set.fromList "abcdef"
  in
    gamble (sublistOf $ Set.toList xs) $ \(ys :: [Char]) ->
      List.all (flip Set.member xs) ys

prop_shuffle :: Property
prop_shuffle =
  gamble (shuffle "abcdef") $ \(xs :: [Char]) ->
    List.sort xs == "abcdef"

prop_listOf1 :: Property
prop_listOf1 =
  gamble (mapTree duplicate . listOf1 $ pure ("x" :: [Char])) $
    -- This might seem silly, but we're really just testing that the "internal
    -- error" case doesn't come up.
    List.all (\xs -> NonEmpty.length xs > 0) . List.take 1000 . Foldable.toList

prop_vectorOf :: Property
prop_vectorOf =
  gamble (choose (0, 1000)) $ \n ->
  gamble (mapTree duplicate . vectorOf n $ pure ("x" :: [Char])) $
    Foldable.all (\xs -> n == List.length xs)

prop_suchThat :: Property
prop_suchThat =
  gamble (mapTree duplicate $ bounded `suchThat` even) $ \(xs :: Tree Int) ->
    List.all even . List.take 1000 $ Foldable.toList xs

return []
tests :: IO Bool
tests =
  $forAllProperties . quickCheckWithResult $ stdArgs { maxSuccess = 100 }
