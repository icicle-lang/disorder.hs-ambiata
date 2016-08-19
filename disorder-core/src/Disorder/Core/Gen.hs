module Disorder.Core.Gen (
    GenSeed(..)
  , chooseSize
  , genDeterministic
  , genDeterministic'
  , genEnum
  , genFromMaybe
  , listOfSized
  , listOfSizedWithIndex
  , listWithIndex
  , maybeGen
  , oneofSized
  , smaller
  , vectorOfSize
  , vectorOfUnique
  , vectorOfUnique'
  , vectorOfUniqueBy
  , vectorOfUniqueBy'
  , listOf1Unique

  -- * re-exports from quickcheck-text
  , genValidUtf8
  , genValidUtf81
  , shrinkValidUtf8
  , shrinkValidUtf81
  , shrinkUtf8BS
  , shrinkUtf8BS1
  , utf8BS
  , utf8BS1
  ) where

import           Control.Applicative

import           Data.Maybe (isJust)
import           Data.Monoid ((<>))

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random
import           Test.QuickCheck.Utf8

import           Prelude

-- | Return a vector whose size is within the provided bounds
vectorOfSize :: Int -> Int -> Gen a -> Gen [a]
vectorOfSize min' max' gen =
  chooseSize min' max' >>= flip vectorOf gen

-- | Return an 'Int' which is between the provided range, and influenced by the current "size"
chooseSize :: Int -> Int -> Gen Int
chooseSize min' max' =
  sized (return . min max' . max min')

-- | from a generator return a generator that will generate Nothing sometimes
maybeGen :: Gen a -> Gen (Maybe a)
maybeGen g = sized $ \s ->
  frequency [
    (1, return Nothing),
    (s, Just <$> resize (s `div` 2) g)]

-- | Wait for a generated `Just` value
--
-- Use _only_ in case of emergencies when you have no other way to get an `a` safely
genFromMaybe :: Gen (Maybe a) -> Gen a
genFromMaybe g =
  suchThat g isJust >>= \ma ->
    case ma of
      Just a -> pure a
      Nothing -> fail "Disorder.Core.Gen.genFromMaybe: Failed to generate a Just"

-- | Generate something smaller
smaller :: Gen a -> Gen a
smaller g =
  sized $ \s -> resize (s `div` 2) g

-- | Take list of small generators and list of large generators.
-- Look at the size of what we want to create, and use either small or both.
--
-- This is useful for generators for recursive datatypes: one can pass generators
-- for the leaf nodes as the first argument, and branches for the second argument.
-- The second arguments will be called with a smaller size, so if this is used
-- recursively, the size will continue to reduce until only leaves are available.
--
-- For example, a tree might use:
--
-- > gen_tree = oneofSized
-- >  [ Leaf1 <$> arbitrary, Leaf2 <$> arbitrary ]
-- >  [ Branch <$> gen_tree ]
--
-- Because (Branch <$> gen_tree) will only be called when the size is greater than 1,
-- and gen_tree will be called with a smaller size, this makes an infinite chain
-- of branches impossible.
oneofSized :: [Gen a] -> [Gen a] -> Gen a
oneofSized smalls bigs = sized $ \s ->
  if   s <= 1
  then oneof  smalls
  else oneof (smalls ++ bigs')
 where
  bigs'   = fmap smaller bigs

-- | Generate a list this big.
listOfSized :: Gen a -> Int -> Gen [a]
listOfSized gen n = take n <$> infiniteListOf gen

genEnum :: (Bounded a, Enum a) => Gen a
genEnum =
  elements [minBound..maxBound]

listWithIndex :: (Int -> Gen a) -> Gen [a]
listWithIndex g =
  sized $ \i -> listOfSizedWithIndex 0 i g

listOfSizedWithIndex :: Int -> Int -> (Int -> Gen a) -> Gen [a]
listOfSizedWithIndex from to g =
  chooseSize from to >>=
    mapM g . enumFromTo 0

newtype GenSeed =
  GenSeed {
    unGenSeed :: Int
  } deriving (Eq, Show)

-- | Deterministic generator with a default size of 100.
genDeterministic :: GenSeed -> Gen a -> a
genDeterministic = genDeterministic' 100

-- | Deterministic generator, always produces the same output for the same
-- seed.
genDeterministic' :: Int -> GenSeed -> Gen a -> a
genDeterministic' size (GenSeed seed) (MkGen g) =
  let r = mkQCGen seed in
  g r size

-- | Generate a list of a given length containing no duplicates (by the
-- provided comparison function). This is 'vectorOfUnique'' parameterised by
-- equality operator; the same caveats apply, including partiality.
vectorOfUniqueBy' :: (a -> a -> Bool) -> Int -> Int -> Gen a -> Gen [a]
vectorOfUniqueBy' cmp s k g =
  scaled (max s) $ go k [] mana
  where
    go 0 xs _ = pure xs
    go _ xs 0 =
      fail $ "vectorOfUniqueBy' out of mana - are you trying to generate a list larger than the cardinality of the type? (" <> show (length xs) <> "/" <> show k <> ")"
    go n xs mana' =
      g >>= \y -> case any (cmp y) xs of
        True -> go n xs $ mana' - 1
        False -> go (n - 1) (y : xs) mana

    mana :: Int
    mana = 10000

    -- scale is in QuickCheck 2.8
    scaled f h = sized $ \x -> resize (f x) h

-- | Generate a list of a given length containing no duplicates (by the
-- provided comparison function). This is 'vectorOfUnique' parameterised by
-- equality operator; the same caveats apply, including partiality.
vectorOfUniqueBy :: (a -> a -> Bool) -> Int -> Gen a -> Gen [a]
vectorOfUniqueBy cmp = vectorOfUniqueBy' cmp 30

-- | Generate a list of a given length containing no duplicates.
--
-- As the provided length can be greater than the cardinality of the type
-- (number of distinct representable values), this function is not guaranteed
-- to terminate successfully; it will give up after 10000 consecutive failures
-- to generate a new value.
--
-- `vectorOfUnique' n` runs the generator with a size equal to the maximum of
-- `n` and the current size parameter; setting this above zero is necessary to
-- use this with sized generators from the QuickCheck test runner. The optimal
-- value depends on the cardinality of the type, how the generator in question
-- uses the size parameter, and the desired number of unique values.
vectorOfUnique' :: Eq a => Int -> Int -> Gen a -> Gen [a]
vectorOfUnique' = vectorOfUniqueBy' (==)

-- | Generate a list of a given length containing no duplicates. This is
-- 'vectorOfUnique'' with a minimum generator size of 30, which is sufficient
-- for most common sized generators.
--
-- This generator is not guaranteed to terminate successfully; see
-- 'vectorOfUnique'' for details.
vectorOfUnique :: Eq a => Int -> Gen a -> Gen [a]
vectorOfUnique = vectorOfUnique' 30

-- | Generates a non-empty list of random length with no duplicate values.
-- The maximum length depends on the size parameter.
--
-- This generator is not guaranteed to terminate successfully; see
-- 'vectorOfUnique'' for details.
listOf1Unique :: Eq a => Gen a -> Gen [a]
listOf1Unique g = sized $ \s -> choose (1, max 1 s) >>= (flip vectorOfUnique g)
