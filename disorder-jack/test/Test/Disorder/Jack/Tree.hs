{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Jack.Tree where

import           Control.Applicative (Applicative(..))
import           Control.Comonad (duplicate)
import           Control.Monad (Monad(..), ap)

import           Data.Bool (Bool)
import           Data.Eq (Eq(..))
import           Data.Functor (Functor(..))
import           Data.Function (($), (.))

import           Disorder.Jack.Combinators
import           Disorder.Jack.Core
import           Disorder.Jack.Property
import           Disorder.Jack.Tree

import           System.IO (IO)

import           Text.Show (Show)
import           Text.Show.Pretty (ppShow)


prop_ap :: Property
prop_ap =
  gamble (treeOf $ chooseInt (1, 5)) $ \x ->
  gamble (treeOf $ chooseChar ('a', 'e')) $ \y ->
    law_ap x y

treeOf :: Jack a -> Jack (Tree a)
treeOf =
  mapTree duplicate

law_ap :: (Show a, Show b, Eq a, Eq b) => Tree a -> Tree b -> Property
law_ap x y =
  let
    s = (,) `fmap` x <*> y
    t = (,) `fmap` x `ap` y
  in
    counterexample "=== Left ===" .
    counterexample (ppShow s) .
    counterexample "=== Right ===" .
    counterexample (ppShow t) $
      s == t

return []
tests :: IO Bool
tests =
  $forAllProperties . quickCheckWithResult $ stdArgs { maxSuccess = 1000 }
