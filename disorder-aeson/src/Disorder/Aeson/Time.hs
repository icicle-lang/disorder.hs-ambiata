module Disorder.Aeson.Time (
    genTime
  ) where

import           Control.Applicative

import           Data.Time

import           Test.QuickCheck

import           Prelude

genTime :: Gen UTCTime
genTime = 
  let
    -- Add one for leap seconds.
    maxDayMilliseconds = 60 * 60 * 24 * 1000 + 1000
  in
  UTCTime
    <$> fmap ModifiedJulianDay arbitrary
    <*> fmap (picosecondsToDiffTime . (*) 1000000000) (choose (0, maxDayMilliseconds))
