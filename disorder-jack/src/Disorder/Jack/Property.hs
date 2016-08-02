{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
module Disorder.Jack.Property (
    gamble
  , gambleRender
  , gambleDisplay

  , shrinking

  , sample
  , sampleTree
  , printSample
  , printSampleTree

  , (===)

  -- * "Test.QuickCheck" re-exports

  -- ** "Test.QuickCheck.Property"
  , Property(..)
  , Testable(..)
  , counterexample
  , failed
  , succeeded
  , rejected
  , (==>)
  , (.&&.)
  , (.||.)
  , conjoin
  , disjoin
  , once

  -- ** "Test.QuickCheck.Exception"
  , discard

  -- ** "Test.QuickCheck.Test"
  , Args(..)
  , stdArgs
  , quickCheck
  , quickCheckWithResult
  , verboseCheckWithResult

  -- ** "Test.QuickCheck.All"
  , quickCheckAll
  , verboseCheckAll
  , forAllProperties
  ) where

import           Data.Eq (Eq(..))
import           Data.Foldable (for_, traverse_)
import           Data.Function (($), (.))
import           Data.Functor (Functor(..))
import qualified Data.List as List
import           Data.Monoid ((<>))
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T

import           Disorder.Jack.Core
import           Disorder.Jack.Tree
import           Disorder.Jack.Property.Diff
import           System.IO (IO, putStrLn)

import           Text.Show (Show)
import           Text.Show.Pretty (ppShow)
import qualified Text.Show.Pretty as Pretty
import           Prelude (Bool(..), Maybe(..))

import qualified Test.QuickCheck as QC
import           Test.QuickCheck.All (quickCheckAll, verboseCheckAll, forAllProperties)
import           Test.QuickCheck.Exception (discard)
import           Test.QuickCheck.Gen.Unsafe (promote)
import           Test.QuickCheck.Property ((==>), (.&&.), (.||.))
import           Test.QuickCheck.Property (Testable(..), Property(..), Prop(..), Rose(..))
import           Test.QuickCheck.Property (conjoin, disjoin, once)
import           Test.QuickCheck.Property (joinRose, counterexample)
import           Test.QuickCheck.Property (succeeded, failed, rejected)
import           Test.QuickCheck.Test (Args(..), stdArgs)
import           Test.QuickCheck.Test (quickCheck, quickCheckWithResult, verboseCheckWithResult)


-- | Ask 'Jack' to generate test cases to exercise the given property.
gamble :: (Show a, Testable prop) => Jack a -> (a -> prop) -> Property
gamble =
  gambleDisplay ppShow

-- | Ask 'Jack' to generate test cases, but provide a custom render function
--   for displaying counterexampes.
gambleRender :: Testable prop => (a -> Text) -> Jack a -> (a -> prop) -> Property
gambleRender render =
  gambleDisplay (T.unpack . render)

-- | Ask 'Jack' to generate test cases, but provide a custom render function
--   for displaying counterexampes.
gambleDisplay :: Testable prop => (a -> String) -> Jack a -> (a -> prop) -> Property
gambleDisplay render jack pf =
  MkProperty $ do
    tree <- runJack jack
    unProperty . shrinking tree $ \x ->
      counterexample (render x) $
      pf x

-- | Use an existing 'Tree' to exercise a given property.
shrinking :: Testable prop => Tree a -> (a -> prop) -> Property
shrinking tree pf =
  let
    props x =
      MkRose
        (unProperty . property . pf $ outcome x)
        (fmap props $ shrinks x)
  in
    MkProperty .
      fmap (MkProp . joinRose . fmap unProp) $
      promote (props tree)

-- | Generate some example outcomes.
sample :: Jack a -> IO [a]
sample =
  fmap (fmap outcome) . QC.sample' . runJack

-- | Generate some example trees.
sampleTree :: Jack a -> IO [Tree a]
sampleTree =
  QC.sample' . runJack

-- | Generate some example outcomes (and shrinks) and prints them to 'stdout'.
printSample :: Show a => Jack a -> IO ()
printSample jack = do
  forest <- fmap (List.take 5) $ sampleTree jack
  for_ forest $ \tree -> do
    putStrLn "=== Outcome ==="
    putStrLn . ppShow $ outcome tree
    putStrLn "=== Shrinks ==="
    traverse_ (putStrLn . ppShow . outcome) $ shrinks tree
    putStrLn ""

printSampleTree :: Show a => Jack a -> IO ()
printSampleTree jack = do
  forest <- fmap (List.take 1) $ sampleTree jack
  for_ forest $ \tree -> do
    putStrLn $ ppShow tree

infix 4 ===

(===) :: (Eq a, Show a) => a -> a -> Property
(===) x y =
  counterexample "=== Not equal ===" $
  counterexample render (x == y)
  where
    render
     | Just x' <- Pretty.reify x
     , Just y' <- Pretty.reify y
     = renderDiffs x' y'
     | True
     = ppShow x <> " /= " <> ppShow y
