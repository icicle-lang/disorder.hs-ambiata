{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.Core.Combinators where

import           Disorder.Core.Combinators
import           Disorder.Core.Run

import           Test.QuickCheck (Property, (===), expectFailure)


prop_testJust_Just :: (Show a, Eq a) => a -> Property
prop_testJust_Just a =
  testJust "just" (Just a) $ \ x -> x === a


prop_testJust_both :: (Show a, Eq a) => Maybe a -> Property
prop_testJust_both ma =
  case ma of
   Just _ -> testJust "just" ma $ \ a -> a === a
   Nothing -> expectFailure . testJust "nothing" ma $ \ a -> a === a


prop_testRight_Right :: (Show a, Eq a) => Either a a -> Property
prop_testRight_Right eaa =
  let ra = case eaa of
            Left a -> Right a
            Right _ -> eaa
  in testRight "right" ra $ \ x -> x === x


prop_testRight_both :: (Show a, Eq a, Show e) => Either e a -> Property
prop_testRight_both ea =
  case ea of
   Right _ -> testRight "right" ea $ \ a -> a === a
   Left _ -> expectFailure . testRight "left" ea $ \ a -> a === a


return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
