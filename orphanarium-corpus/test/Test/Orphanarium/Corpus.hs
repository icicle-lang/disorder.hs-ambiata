{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Orphanarium.Corpus where

import           Control.Applicative

import           Data.Text
import           Data.Monoid

import           Orphanarium.Corpus

import           Test.QuickCheck

-- Not much to test here, this is just an example of using the corpus for maximum humour:

newtype Laugh =
  Laugh Text deriving (Eq, Show)

instance Arbitrary Laugh where
  arbitrary =
    let laugh x y = Laugh $ x <> " " <> y
     in laugh <$> elements cooking <*> elements muppets

prop_corpusUsage :: Laugh -> Bool
prop_corpusUsage l =
  let isFunny = const True in isFunny l

return []
tests :: IO Bool
tests = $quickCheckAll
