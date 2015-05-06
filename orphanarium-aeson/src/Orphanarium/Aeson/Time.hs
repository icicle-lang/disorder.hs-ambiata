module Orphanarium.Aeson.Time (
    genTime
  ) where

import           Control.Applicative

import           Data.Time

import           Test.QuickCheck


genTime :: Gen UTCTime
genTime = UTCTime
  <$> fmap ModifiedJulianDay arbitrary
  <*> fmap (picosecondsToDiffTime . (*) 1000000000) (choose (0, 60 * 60 * 24 * 1000))
