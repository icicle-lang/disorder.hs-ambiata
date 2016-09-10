{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Disorder.Jack.Combinators (
  -- * Shrink Modifiers
    noShrink

  -- * Size Combinators
  , variant
  , sized
  , resize
  , scale

  -- * Sized Generators
  , sizedInt
  , sizedIntegral
  , sizedNat
  , sizedNatural
  , sizedBounded

  -- * Bounded Generators
  , bounded
  , boundedInt
  , boundedEnum

  -- * Range Generators
  , choose
  , chooseInt
  , chooseChar

  -- * List Combinators
  , oneOf
  , oneOfRec
  , frequency
  , elements
  , sublistOf
  , shuffle
  , listOf
  , listOf1
  , listOfN
  , vectorOf

  -- * Uncertainty Combinators
  , maybeOf
  , justOf
  , suchThat
  , suchThatMaybe

  -- * QuickCheck Compatibility
  , arbitrary
  ) where

import           Control.Monad (replicateM)
import           Control.Applicative (Applicative(..))

import           Data.Bool (Bool(..), not)
import           Data.Char (Char, ord, chr)
import           Data.Function (($), (.), id)
import           Data.Functor (Functor(..), (<$>))
import           Data.Int (Int)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (Maybe(..), isJust)
import           Data.Monoid ((<>))
import           Data.Ord (Ord(..))
import           Data.Tuple (fst)

import           Disorder.Jack.Core
import           Disorder.Jack.Shrink
import           Disorder.Jack.Tree

import           Prelude (Num(..), Bounded(..), Enum(..), Integral, div)
import qualified Prelude as Savage

import           System.Random (Random)

import qualified Test.QuickCheck as QC


-- | Prevent a 'Jack' from shrinking.
noShrink :: Jack a -> Jack a
noShrink =
  mapTree $ \(Node x _) ->
    Node x []

-- | Modifies a generator using an integer seed.
variant :: Integral n => n -> Jack a -> Jack a
variant =
  mapGen . QC.variant

-- | Construct a 'Jack' that depends on the size parameter.
sized :: (Int -> Jack a) -> Jack a
sized f =
  Jack $ QC.sized (runJack . f)

-- | Overrides the size parameter. Returns a 'Jack' which uses the given size
--   instead of the runtime-size parameter.
resize :: Int -> Jack a -> Jack a
resize n =
  if n < 0 then
    Savage.error "Disorder.Jack.Combinators.resize: negative size"
  else
    mapGen $ QC.resize n

-- | Update the current size by mapping a function over it.
scale :: (Int -> Int) -> Jack a -> Jack a
scale f j =
  sized $ \n ->
    resize (f n) j

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

-- | Generates an integral number from a bounded domain. The number is chosen
--   from the entire range of the type, but small numbers are generated more
--   often than big numbers.
sizedBounded :: (Bounded a, Integral a) => Jack a
sizedBounded =
  mkJack QC.shrinkIntegral QC.arbitrarySizedBoundedIntegral

-- | Generates an integral number. The number is chosen from the entire range
--   of the type.
bounded :: (Bounded a, Random a, Integral a) => Jack a
bounded =
  mkJack (shrinkTowards 0) $ QC.choose (minBound, maxBound)

-- | Generates an 'Int'. The number is chosen from the entire range of valid
--   'Int' values, on 64-bit GHC this is [-2^63, 2^63).
boundedInt :: Jack Int
boundedInt =
  bounded

-- | Generates an element from a bounded enumeration.
boundedEnum :: forall a. (Bounded a, Enum a) => Jack a
boundedEnum =
  let
    e_min = minBound :: a
    e_max = maxBound :: a
  in
    fmap toEnum $ chooseInt (fromEnum e_min, fromEnum e_max)

-- | Generates an integral number in the given range.
choose :: (Random a, Integral a) => (a, a) -> Jack a
choose (b0, b1) =
  let
    b_min =
      min b0 b1

    b_max =
      max b0 b1
  in
    mkJack (shrinkTowards b_min) $ QC.choose (b_min, b_max)

-- | Generates an 'Int' in the given range.
chooseInt :: (Int, Int) -> Jack Int
chooseInt =
  choose

-- | Generates a 'Char' in the given range.
chooseChar :: (Char, Char) -> Jack Char
chooseChar (b0, b1) =
  fmap chr $ choose (ord b0, ord b1)

-- | Randomly selects one of the jacks in the list.
--   /The input list must be non-empty./
oneOf :: [Jack a] -> Jack a
oneOf = \case
  [] ->
    Savage.error "Disorder.Jack.Combinators.oneOf: used with empty list"
  xs -> do
    n <- choose (0, List.length xs - 1)
    xs List.!! n

-- | Randomly selects from one of the jacks in either the non-recursive or the
--   recursive list. When a selection is made from the recursive list, the size
--   is halved. When the size gets to one or less, selections are no longer made
--   from the recursive list.
--   /The first argument (i.e. the non-recursive input list) must be non-empty./
oneOfRec :: [Jack a] -> [Jack a] -> Jack a
oneOfRec nonrec rec =
  sized $ \n ->
    if n <= 1 then
      oneOf nonrec
    else
      oneOf $ nonrec <> fmap (scale (`div` 2)) rec

-- | Uses a weighted distribution to randomly select one of the jacks in the list.
--   /The input list must be non-empty./
frequency :: [(Int, Jack a)] -> Jack a
frequency = \case
  [] ->
    Savage.error "Disorder.Jack.Combinators.frequency: used with empty list"
  xs0 -> do
    let
      pick n = \case
        [] ->
          Savage.error "Disorder.Jack.Combinators.frequency/pick: used with empty list"
        (k, x) : xs ->
          if n <= k then
            x
          else
            pick (n - k) xs

      total =
        List.sum (fmap fst xs0)

    n <- choose (1, total)
    pick n xs0

-- | Randomly selects one of the values in the list.
--   /The input list must be non-empty./
elements :: [a] -> Jack a
elements = \case
  [] ->
    Savage.error "Disorder.Jack.Combinators.elements: used with empty list"
  xs -> do
    n <- choose (0, List.length xs - 1)
    pure $ xs List.!! n

-- | Generates a random subsequence of the given list.
sublistOf :: [a] -> Jack [a]
sublistOf =
  Jack . fmap (unfoldTree id shrinkList) . QC.sublistOf

-- | Generates a random permutation of the given list.
--
--   This shrinks towards the order of the list being identical to the input
--   list.
--
shuffle :: [a] -> Jack [a]
shuffle = \case
  [] ->
    pure []
  xs0 -> do
    n <- choose (0, List.length xs0 - 1)
    case List.splitAt n xs0 of
      (xs, y : ys) ->
        (y :) <$> shuffle (xs <> ys)
      (_, []) ->
        Savage.error "Disorder.Jack.Combinators.shuffle: internal error, split generated empty list"

-- | Generates a list of random length. The maximum length depends on the size
--   parameter.
listOf :: Jack a -> Jack [a]
listOf jack =
  sized $ \n -> do
    Jack $ do
      k <- QC.choose (0, n)
      fmap sequenceShrinkList . replicateM k $ runJack jack

-- | Generates a non-empty list of random length. The maximum length depends on
--   the size parameter.
listOf1 :: Jack a -> Jack (NonEmpty a)
listOf1 jack =
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

-- | Generates a list between 'n' and 'm' in length.
listOfN :: Int -> Int -> Jack a -> Jack [a]
listOfN n m (Jack gen) =
  Jack $ do
    k <- QC.choose (n, m)

    let
      k_min =
        min n m

      check xs =
        List.length xs >= k_min

    fmap (filterTree check . sequenceShrinkList) $
      replicateM k gen

-- | Generates a list of the given length.
vectorOf :: Int -> Jack a -> Jack [a]
vectorOf n =
  mapGen (fmap sequenceShrinkOne . replicateM n)

-- | Generates a 'Nothing' some of the time.
maybeOf :: Jack a -> Jack (Maybe a)
maybeOf jack =
  sized $ \n ->
    frequency [
        (1, pure Nothing)
      , (1 + n, Just <$> jack)
      ]

-- | Runs a generator that produces 'Maybe a' until it produces a 'Just'.
justOf :: Jack (Maybe a) -> Jack a
justOf g = do
  mx <- suchThat g isJust
  case mx of
    Just x ->
      pure x
    Nothing ->
      Savage.error "Disorder.Jack.Combinators.justOf: internal error, unexpected Nothing"

-- | Generates a value that satisfies a predicate.
suchThat :: Jack a -> (a -> Bool) -> Jack a
suchThat (Jack gen) p =
  Jack $
    let
      loop = do
        mx <- tryGen gen p
        case mx of
          Just x ->
            pure x
          Nothing ->
            QC.sized $ \n ->
              QC.resize (n + 1) loop
    in
      loop

-- | Tries to generate a value that satisfies a predicate.
suchThatMaybe :: Jack a -> (a -> Bool) -> Jack (Maybe a)
suchThatMaybe (Jack gen) p =
  Jack $ do
    mx <- tryGen gen p
    case mx of
      Nothing ->
        pure (pure Nothing)
      Just x ->
        pure (fmap Just x)

-- More or less the same logic as suchThatMaybe from QuickCheck, except
-- modified to ensure that the shrinks also obey the predicate.
tryGen :: QC.Gen (Tree a) -> (a -> Bool) -> QC.Gen (Maybe (Tree a))
tryGen gen p =
  let
    try k = \case
      0 ->
        pure Nothing
      n -> do
        x <- QC.resize (2 * k + n) gen
        if p (outcome x) then
          pure . Just $ filterTree p x
        else
          try (k + 1) (n - 1)
  in
    QC.sized $ try 0 . max 1

-- | Construct a 'Jack' using a type's QuickCheck 'QC.Arbitrary' instance.
arbitrary :: QC.Arbitrary a => Jack a
arbitrary =
  mkJack QC.shrink QC.arbitrary
