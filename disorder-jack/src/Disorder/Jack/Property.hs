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
import           System.IO (IO, putStrLn)

import           Text.Show (Show)
import           Text.Show.Pretty (ppShow)
import qualified Text.Show.Pretty as Pretty
import           Prelude (Num(..), Ord(..))
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
  counterexample renderDiff (x == y)
  where
    renderDiff
     | Just x' <- Pretty.reify x
     , Just y' <- Pretty.reify y
     = renderDiffs x' y'
     | True
     = ppShow x <> " /= " <> ppShow y

-- | Attempt to render difference between two MaxML values
renderDiffs :: Pretty.Value -> Pretty.Value -> String
renderDiffs val1 val2 = prints $ go 0 val1 val2
 where
  go i a b = case (a, b) of
    _
     -- If values are same, render normally
     | a == b
     -> same i (Pretty.valToStr a)
     -- If values are different but both fit on one line, just print both
     | List.length (List.lines (Pretty.valToStr a)) < 2
     , List.length (List.lines (Pretty.valToStr b)) < 2
     -> ll i a <> rr i b

    -- Both the same constructor with same number of arguments, so check arguments
    (Pretty.Con m us, Pretty.Con n vs)
     | n == m
     , List.length us == List.length vs
     -> same i n <> goes go "" "" "" i (List.zip us vs)

    -- Record constructors, check arguments
    (Pretty.Rec m us, Pretty.Rec n vs)
     | n == m
     , List.length us == List.length vs
     , fmap (\(u,_) -> u) us == fmap (\(v,_) -> v) vs
     -> same i n
     -- Print field name too
     <> goes (\i' (ix,u) (_,v) -> same i' (ix <> " = ") <> go i' u v) "{" "," "}" i (List.zip us vs)

    -- Tuples and lists of same length
    (Pretty.Tuple us, Pretty.Tuple vs)
     | List.length us == List.length vs
     -> goes go "(" "," ")" i (List.zip us vs)

    (Pretty.List us, Pretty.List vs)
     | List.length us == List.length vs
     -> goes go "[" "," "]" i (List.zip us vs)

    -- Otherwise they are different constructors and we can't descend further, so print both
    _
     -> ll i a <> rr i b

  -- Print same
  same i n = sho " " i n
  -- Print value on left side
  ll i v = sho "-" i (Pretty.valToStr v)
  -- Print value on right side
  rr i v = sho "+" i (Pretty.valToStr v)

  -- Split up multi-line things into same indentation level.
  -- Any indentation in the string itself will still be there as spaces.
  sho pre i val = fmap (\a -> (pre,i,a)) $ List.lines val

  goes gg l m r i uvs
   = same i l <> goes' False gg m i uvs <> same i r

  goes' putSep gg m i ((u,v):uvs)
   = let rest = gg (i+1) u v <> goes' True gg m i uvs
     in if   putSep
        then same i m <> rest
        else rest
  goes' _ _ _ _ []
   = []

  -- Layout / printing part
  prints [] = []
  -- Squash two lines together.
  -- If the next line is indented more, we might be able to squeeze them onto a single line.
  -- This looks a bit nicer.
  -- For example,
  -- "    ,"
  -- "       Foo"
  -- can be condensed onto a single line:
  -- "    ,  Foo"
  --
  prints ((p1,i1,v1):(p2,i2,v2):fs)
   -- Only if their '+' or '-' prefix are the same
   | p1 == p2
   -- Get end of first line
   , end1   <- i1 * tabSize + List.length v1
   -- And start of next line
   , start2 <- i2 * tabSize
   -- Check end fits before start
   , end1 < start2
   -- Now add extra padding that we lost by condensing, to get to second line's indentation level
   = let diff = start2 - end1
     in  prints ((p1,i1,v1 <> List.replicate diff ' ' <> v2) : fs)

  -- Indent and append all the bits together
  prints ((p,i,v):fs)
   = let tabs = List.replicate (i * tabSize) ' '
     in  p <> tabs <> v <> "\n" <> prints fs

  tabSize = 2
