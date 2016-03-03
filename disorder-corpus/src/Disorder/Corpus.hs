{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Disorder.Corpus (
    genCorpus
  , shrinkCorpus

  , Cooking(..)
  , unCooking
  , cooking

  , Muppet(..)
  , unMuppet
  , muppets

  , Southpark(..)
  , unSouthpark
  , southpark

  , Simpson(..)
  , unSimpson
  , simpsons

  , Virus(..)
  , unVirus
  , viruses

  , Colour(..)
  , unColour
  , colours

  , Weather(..)
  , unWeather
  , weather

  , Water(..)
  , unWater
  , waters

  , Boat(..)
  , unBoat
  , boats
  ) where

import           Data.Data (Data)
import           Data.Eq (Eq)
import           Data.Functor (fmap)
import qualified Data.List as List
import           Data.Monoid ((<>))
import           Data.Ord (Ord)
import           Data.String (IsString(..))
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           Test.QuickCheck (Arbitrary(..), Gen, oneof, elements)

import           Text.Show (Show)
import           Text.Read (Read)


-- | Generate something in the corpus or something completely bonkers.
genCorpus :: [a] -> (a -> b) -> Gen a -> Gen b
genCorpus corpus f gen =
  oneof [
      fmap f (elements corpus)
    , fmap f gen
    ]

-- | Shrinks 'b', preferring to return things from the corpus. If 'b' is
--   already from the corpus then shrinks to nothing.
shrinkCorpus :: Eq a => [a] -> (a -> b) -> (b -> a) -> (a -> [a]) -> b -> [b]
shrinkCorpus corpus f g shrinkA b =
  let
    a = g b
  in
    if List.elem a corpus then
      []
    else
      -- Items at the start of the shrink list are tried first by QuickCheck,
      -- so put the corpus items first and hopefully we'll get a nicer
      -- counterexample.
      fmap f (corpus <> shrinkA a)

--
-- The newtypes below have the unXXX function defined separately so that
-- the derived Show instance produces output which is easier to read.
--

newtype Cooking a =
  Cooking a
  deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)

unCooking :: Cooking a -> a
unCooking (Cooking x) =
  x

instance (Eq a, IsString a, Arbitrary a) => Arbitrary (Cooking a) where
  arbitrary =
    genCorpus cooking Cooking arbitrary
  shrink =
    shrinkCorpus cooking Cooking unCooking shrink

cooking :: IsString a => [a]
cooking = [
    "salted"
  , "stewed"
  , "diced"
  , "filleted"
  , "sauteed"
  ]


newtype Muppet a =
  Muppet a
  deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)

unMuppet :: Muppet a -> a
unMuppet (Muppet x) =
  x

instance (Eq a, IsString a, Arbitrary a) => Arbitrary (Muppet a) where
  arbitrary =
    genCorpus muppets Muppet arbitrary
  shrink =
    shrinkCorpus muppets Muppet unMuppet shrink

muppets :: IsString a => [a]
muppets = [
    "kermit"
  , "gonzo"
  , "fozzy"
  , "chef"
  , "statler"
  , "waldorf"
  , "beaker"
  , "animal"
  ]


newtype Southpark a =
  Southpark a
  deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)

unSouthpark :: Southpark a -> a
unSouthpark (Southpark x) =
  x

instance (Eq a, IsString a, Arbitrary a) => Arbitrary (Southpark a) where
  arbitrary =
    genCorpus southpark Southpark arbitrary
  shrink =
    shrinkCorpus southpark Southpark unSouthpark shrink

southpark :: IsString a => [a]
southpark = [
    "kyle"
  , "stan"
  , "cartman"
  , "timmy"
  , "token"
  , "chef"
  , "garrison"
  ]


newtype Simpson a =
  Simpson a
  deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)

unSimpson :: Simpson a -> a
unSimpson (Simpson x) =
  x

instance (Eq a, IsString a, Arbitrary a) => Arbitrary (Simpson a) where
  arbitrary =
    genCorpus simpsons Simpson arbitrary
  shrink =
    shrinkCorpus simpsons Simpson unSimpson shrink

simpsons :: IsString a => [a]
simpsons = [
    "homer"
  , "marge"
  , "maggie"
  , "lisa"
  , "bart"
  , "flanders"
  , "moe"
  , "barney"
  ]


newtype Virus a =
  Virus a
  deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)

unVirus :: Virus a -> a
unVirus (Virus x) =
  x

instance (Eq a, IsString a, Arbitrary a) => Arbitrary (Virus a) where
  arbitrary =
    genCorpus viruses Virus arbitrary
  shrink =
    shrinkCorpus viruses Virus unVirus shrink

viruses :: IsString a => [a]
viruses = [
    "rotavirus"
  , "smallpox"
  , "norovirus"
  , "chickenpox"
  , "camelpox"
  , "dengue"
  , "echovirus"
  , "equine morbillivirus"
  , "gou virus"
  , "measles"
  , "monkeypox"
  ]


newtype Colour a =
  Colour a
  deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)

unColour :: Colour a -> a
unColour (Colour x) =
  x

instance (Eq a, IsString a, Arbitrary a) => Arbitrary (Colour a) where
  arbitrary =
    genCorpus colours Colour arbitrary
  shrink =
    shrinkCorpus colours Colour unColour shrink

colours :: IsString a => [a]
colours = [
    "red"
  , "green"
  , "blue"
  , "yellow"
  , "black"
  , "grey"
  , "purple"
  , "orange"
  , "pink"
  ]


newtype Weather a =
  Weather a
  deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)

unWeather :: Weather a -> a
unWeather (Weather x) =
  x

instance (Eq a, IsString a, Arbitrary a) => Arbitrary (Weather a) where
  arbitrary =
    genCorpus weather Weather arbitrary
  shrink =
    shrinkCorpus weather Weather unWeather shrink

weather :: IsString a => [a]
weather = [
    "dry"
  , "raining"
  , "hot"
  , "humid"
  , "snowing"
  , "fresh"
  , "windy"
  , "freezing"
  ]


newtype Water a =
  Water a
  deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)

unWater :: Water a -> a
unWater (Water x) =
  x

instance (Eq a, IsString a, Arbitrary a) => Arbitrary (Water a) where
  arbitrary =
    genCorpus waters Water arbitrary
  shrink =
    shrinkCorpus waters Water unWater shrink

waters :: IsString a => [a]
waters = [
    "basin"
  , "bay"
  , "billabong"
  , "canal"
  , "channel"
  , "creek"
  , "estuary"
  , "fjord"
  , "harbour"
  , "lake"
  , "loch"
  , "marsh"
  , "ocean"
  , "pond"
  , "puddle"
  , "reservoir"
  , "river"
  , "sea"
  , "slough"
  , "sound"
  , "spring"
  , "stream"
  , "swamp"
  , "wetland"
  ]


newtype Boat a =
  Boat a
  deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)

unBoat :: Boat a -> a
unBoat (Boat x) =
  x

instance (Eq a, IsString a, Arbitrary a) => Arbitrary (Boat a) where
  arbitrary =
    genCorpus boats Boat arbitrary
  shrink =
    shrinkCorpus boats Boat unBoat shrink

boats :: IsString a => [a]
boats = [
    "barge"
  , "battleship"
  , "canoe"
  , "catamaran"
  , "dinghy"
  , "ferry"
  , "gondola"
  , "jetski"
  , "kayak"
  , "longship"
  , "motorboat"
  , "pontoon"
  , "powerboat"
  , "rowboat"
  , "ship"
  , "steamboat"
  , "tanker"
  , "trawler"
  , "tugboat"
  , "yacht"
  ]
