{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Disorder.Jack.Combinators (
    arbitrary

  , sized
  , sizedInt
  , sizedIntegral
  , sizedNat
  , sizedNatural

  , chooseChar
  , chooseInt
  , chooseIntegral

  , oneof
  , elements
  , list
  , list1
  , vector
  ) where

import           Control.Monad (replicateM)
import           Control.Applicative (Applicative(..))

import           Data.Bool (not)
import           Data.Char (Char, ord, chr)
import           Data.Function (($), (.))
import           Data.Functor (Functor(..))
import           Data.Int (Int)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import           Disorder.Jack.Core
import           Disorder.Jack.Shrink
import           Disorder.Jack.Tree

import           Prelude (Num(..), Integral, min, max)
import qualified Prelude as Savage

import           System.Random (Random)

import qualified Test.QuickCheck as QC


-- | Construct a 'Jack' using a type's QuickCheck 'QC.Arbitrary' instance.
arbitrary :: QC.Arbitrary a => Jack a
arbitrary =
  mkJack QC.shrink QC.arbitrary

-- | Construct a 'Jack' that depends on the size parameter.
sized :: (Int -> Jack a) -> Jack a
sized f =
  Jack $ QC.sized (runJack . f)

-- | Generates an 'Int'. The number can be positive or negative and its maximum
--   absolute value depends on the size parameter.
sizedInt :: Jack Int
sizedInt =
  sizedIntegral

-- | Generates an integral number. The number can be positive or negative and
--   its maximum absolute value depends on the size parameter.
sizedIntegral :: Integral a => Jack a
sizedIntegral =
  mkJack QC.shrinkIntegral QC.arbitrarySizedIntegral

-- | Generates a non-negative 'Int'. The number's maximum value depends on the
--   size parameter.
sizedNat :: Jack Int
sizedNat =
  sizedNatural

-- | Generates a natural number. The number's maximum value depends on
--   the size parameter.
sizedNatural :: Integral a => Jack a
sizedNatural =
  mkJack QC.shrinkIntegral QC.arbitrarySizedNatural

-- | Generates a 'Char' in the given range.
chooseChar :: Char -> Char -> Jack Char
chooseChar b0 b1 =
  fmap chr $ chooseIntegral (ord b0) (ord b1)

-- | Generates an 'Int' in the given range.
chooseInt :: Int -> Int -> Jack Int
chooseInt =
  chooseIntegral

-- | Generates an integral number in the given range.
chooseIntegral :: (Random a, Integral a) => a -> a -> Jack a
chooseIntegral b0 b1 =
  let
    b_min =
      min b0 b1

    b_max =
      max b0 b1
  in
    mkJack (shrinkIntegral b_min) $ QC.choose (b_min, b_max)

-- | Randomly selects one of jacks in the list.
--   /The input list must be non-empty./
oneof :: [Jack a] -> Jack a
oneof = \case
  [] ->
    Savage.error "Disorder.Jack.Combinators.oneof: used with empty list"
  xs -> do
    n <- chooseInt 0 (List.length xs - 1)
    xs List.!! n

-- | Randomly selects one of the values in the list.
--   /The input list must be non-empty./
elements :: [a] -> Jack a
elements = \case
  [] ->
    Savage.error "Disorder.Jack.Combinators.elements: used with empty list"
  xs -> do
    n <- chooseInt 0 (List.length xs - 1)
    pure $ xs List.!! n

-- | Generates a list of random length. The maximum length depends on the size
--   parameter.
list :: Jack a -> Jack [a]
list jack =
  sized $ \n -> do
    Jack $ do
      k <- QC.choose (0, n)
      fmap sequenceShrinkList . replicateM k $ runJack jack

-- | Generates a non-empty list of random length. The maximum length depends on
--   the size parameter.
list1 :: Jack a -> Jack (NonEmpty a)
list1 jack =
  sized $ \n -> do
    Jack $ do
      k <- QC.choose (1, max n 1)

      let
        unpack = \case
          [] ->
            Savage.error "Disorder.Jack.Combinators.list1: internal error, generated empty list"
          xs ->
            NonEmpty.fromList xs

        go =
          fmap unpack .
          filterTree (not . List.null) .
          sequenceShrinkList

      fmap go . replicateM k $ runJack jack

-- | Generates a list of the given length.
vector :: Int -> Jack a -> Jack [a]
vector n =
  mapGen (fmap sequenceShrinkOne . replicateM n)
