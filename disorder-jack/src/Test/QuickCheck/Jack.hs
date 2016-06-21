{-# LANGUAGE NoImplicitPrelude #-}
-- | Can be used as a drop-in replacement for "Test.QuickCheck" in some cases.
module Test.QuickCheck.Jack (
    Gen
  , forAll
  , arbitraryBoundedEnum
  , module X
  ) where

import           Disorder.Jack as X

import           Prelude (Bounded, Enum)

import           Text.Show (Show)

import           Test.QuickCheck as X (Arbitrary)


type Gen =
  Jack

forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAll =
  gamble

arbitraryBoundedEnum :: (Bounded a, Enum a) => Jack a
arbitraryBoundedEnum =
  boundedEnum
