{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Disorder.Cli.Shell where

import           Control.Applicative

import           Data.Monoid
import qualified Data.Text               as T

import           Disorder.Cli.Shell
import           Disorder.Core.IO

import           System.Exit

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Turtle

import           Prelude

boring :: Gen Text
boring = fmap T.pack . listOf1 $ elements (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'])

prop_testShell :: Property
prop_testShell = forAll (listOf1 boring) $ \ms -> (monadicIO . (=<<) stopIO . run) $ do
  (st, out) <- testShell ["tac"] $ select (toLine ms)
  pure $ (st, out) === (ExitSuccess, flip T.snoc '\n' $ T.intercalate "\n" (reverse ms))

prop_testShell_succ :: Property
prop_testShell_succ = forAll boring $ \m -> (monadicIO . (=<<) stopIO . run) $ do
  (st, out) <- testShell' ["echo", "-n", m]
  pure $ (st, out) === (ExitSuccess, m)

prop_testShell_fail :: Property
prop_testShell_fail = forAll boring $ \m -> (monadicIO . (=<<) stopIO . run) $ do
  (st, out) <- testShell' ["echo", "-n", m, "&&", "false"]
  pure $ (st == ExitSuccess, out) === (False, m)

return []
tests :: IO Bool
tests = $quickCheckAll
