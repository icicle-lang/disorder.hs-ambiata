{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Test.Disorder.Jack.Property.Diff where

import           Control.Monad (Monad(..))

import           Data.Bool (Bool(..))
import           Data.Eq (Eq(..))
import           Data.Function (($), (.))
import qualified Data.List as List
import           Data.Maybe (Maybe(..))
import           Data.Either (Either(..))
import           Data.String (String)

import           Disorder.Jack
import           Disorder.Jack.Property.Diff

import           Text.Show (Show(..))
import qualified Text.Show.Pretty as Pretty

import           Prelude (Int)

import           System.IO (IO)

checkDiff :: Show a => a -> a -> [String] -> Property
checkDiff left right expect =
 let Just left'  = Pretty.reify left
     Just right' = Pretty.reify right
     actual      = renderDiffs left' right'
     expect'     = List.unlines expect
 in  counterexample "=== Left ==="
   $ counterexample (show left)
   $ counterexample "=== Right ==="
   $ counterexample (show right)
   $ counterexample "=== Expect ==="
   $ counterexample expect'
   $ counterexample "=== Actual ==="
   $ counterexample actual
   ( actual == expect' )

-- If the difference fits on single line, the outer constructors should appear in diff
prop_eg1 :: Property
prop_eg1 =
  checkDiff
    (Just 1)
    (Just 2)
    [ "-Just 1"
    , "+Just 2" ]

prop_eg2 :: Property
prop_eg2 =
  checkDiff
    [1,2,3,4]
    [1,2,3,5]
    [ "-[ 1 , 2 , 3 , 4 ]"
    , "+[ 1 , 2 , 3 , 5 ]" ]

-- If we have a big enough list that it won't fit on a single line,
-- it should dig down into the nested parts to show the diff
data SomethingLong
 = SomethingReallyReallyLongThatDontFitOnASingleLine
 deriving Show

prop_eg3 :: Property
prop_eg3 =
  checkDiff
    [Right SomethingReallyReallyLongThatDontFitOnASingleLine,  Left 4]
    [Right SomethingReallyReallyLongThatDontFitOnASingleLine,  Left 5]
    [ " [ Right SomethingReallyReallyLongThatDontFitOnASingleLine"
    , " ,"
    , "-  Left 4"
    , "+  Left 5"
    , " ]"
    ]

-- Records show full field names
data RecordWithFields
 = RecordWithFields
 { field1 :: Int
 , field2 :: Maybe Int
 , field3 :: [RecordWithFields]
 }
 deriving Show

prop_eg4 :: Property
prop_eg4 =
  checkDiff
    (RecordWithFields 0 (Just 1) [RecordWithFields 2 Nothing []])
    (RecordWithFields 0 Nothing  [RecordWithFields 2 (Just 1) []])
    [ " RecordWithFields"
    , " { field1 ="
    , "     0"
    , " , field2 ="
    , "-    Just 1"
    , "+    Nothing"
    , " , field3 ="
    , "     ["
    , "-      RecordWithFields { field1 = 2 , field2 = Nothing , field3 = [] }"
    , "+      RecordWithFields { field1 = 2 , field2 = Just 1 , field3 = [] }"
    , "     ]"
    , " }"
    ]

return []
tests :: IO Bool
tests =
  $forAllProperties . quickCheckWithResult $ stdArgs { maxSuccess = 1 }

