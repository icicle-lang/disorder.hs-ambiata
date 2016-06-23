{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Jack.Tripping where

import           Control.Monad (return)

import           Disorder.Core.Property (neg)
import           Disorder.Jack

import           Data.Bool (Bool)
import           Data.Function (($), (.), id, const)
import           Data.Int (Int)
import           Data.Maybe (Maybe(..))

import           System.IO (IO)


prop_tripping :: Property
prop_tripping =
  gamble bounded $
    tripping id (Just :: Int -> Maybe Int)

prop_tripping_neg :: Property
prop_tripping_neg =
  gamble bounded $
    neg . tripping id (const Nothing :: Int -> Maybe Int)

return []
tests :: IO Bool
tests =
  $quickCheckAll
