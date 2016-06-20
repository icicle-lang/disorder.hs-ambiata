{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Disorder.Jack.Property (
    gamble
  , gambleRender
  , gambleDisplay

  , shrinking

  , sample
  , sampleTree
  , printSample

  -- * QuickCheck re-exports
  , Property(..)
  , Testable(..)
  ) where

import           Data.Foldable (for_, traverse_)
import           Data.Function (($), (.))
import           Data.Functor (Functor(..))
import qualified Data.List as List
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T

import           Disorder.Jack.Core
import           Disorder.Jack.Tree

import           System.IO (IO, putStrLn)

import           Text.Show (Show)
import           Text.Show.Pretty (ppShow)

import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Gen.Unsafe (promote)
import           Test.QuickCheck.Property (Testable(..), Property(..), Prop(..), Rose(..))
import           Test.QuickCheck.Property (joinRose)


-- | Ask 'Jack' to generate test cases to exercise the given property.
gamble :: (Show a, Testable prop) => Jack a -> (a -> prop) -> Property
gamble jack =
  gambleDisplay jack ppShow

-- | Ask 'Jack' to generate test cases, but provide a custom render function
--   for displaying counterexampes.
gambleRender :: Testable prop => Jack a -> (a -> Text) -> (a -> prop) -> Property
gambleRender jack render =
  gambleDisplay jack (T.unpack . render)

-- | Ask 'Jack' to generate test cases, but provide a custom render function
--   for displaying counterexampes.
gambleDisplay :: Testable prop => Jack a -> (a -> String) -> (a -> prop) -> Property
gambleDisplay jack render pf =
  MkProperty $ do
    tree <- runJack jack
    unProperty . shrinking tree $ \x ->
      QC.counterexample (render x) $
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
