{-# LANGUAGE NoImplicitPrelude #-}
-- | Can be used as a drop-in replacement for "Test.QuickCheck" in some cases.
module Test.QuickCheck.Jack (
    Gen
  , forAll
  , arbitraryBoundedEnum
  , oneof
  , listOf1
  , module X
  ) where

import qualified Disorder.Jack as Jack
import           Disorder.Jack as X hiding (listOf1)

import           Data.Function ((.))
import           Data.Functor (fmap)
import           Data.Foldable (toList)

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

oneof :: [Jack a] -> Jack a
oneof =
  oneOf

listOf1 :: Jack a -> Jack [a]
listOf1 =
  fmap toList . Jack.listOf1
