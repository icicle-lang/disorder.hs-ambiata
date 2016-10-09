{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.Jack.Core (
    Jack(..)
  , mkJack
  , mkJack_

  , mapGen
  , mapTree

  , reshrink
  , withShrink
  ) where

import           Control.Applicative (Applicative(..), liftA2)
import           Control.Monad (Monad(..))

import           Data.Function (($), (.), const, flip, id)
import           Data.Functor (Functor(..))

import           Disorder.Jack.Tree

import           System.Random (split)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck.Gen as QC
import           Test.QuickCheck.Random (QCGen)


-- | Jack's love of dice has brought him here, where he has taken on the form
--   of a Haskell library, in order to help you gamble with your properties.
--
newtype Jack a =
  Jack {
      runJack :: Gen (Tree a)
    }

instance Functor Jack where
  fmap f =
    Jack . fmap (fmap f) . runJack

instance Applicative Jack where
  pure =
    Jack . pure . pure

  (<*>) f x =
    Jack $
      liftA2 (<*>) (runJack f) (runJack x)

instance Monad Jack where
  return =
    pure

  (>>=) m0 k0 =
    Jack $ bindGenTree (runJack m0) (runJack . k0)

-- | Used to implement '(>>=)' for 'Jack'.
bindGenTree :: Gen (Tree a) -> (a -> Gen (Tree b)) -> Gen (Tree b)
bindGenTree m k =
  -- It's important to note that we don't use 'traverse' here, we explicitly
  -- only split the seed once, this ensures we get the same behaviour for our
  -- Monad and Applicative instances.
  QC.MkGen $ \seed0 size ->
    let
      (seed1, seed2) =
        split seed0

      runGen :: QCGen -> Gen x -> x
      runGen seed gen =
        QC.unGen gen seed size
    in
      runGen seed1 m >>= runGen seed2 . k

-- | Create a 'Jack' from a shrink function and a 'Gen'.
mkJack :: (a -> [a]) -> Gen a -> Jack a
mkJack shr =
  Jack . fmap (unfoldTree id shr)

-- | Create a non-shrinking 'Jack' from a 'Gen'.
mkJack_ :: Gen a -> Jack a
mkJack_ =
  mkJack $ const []

-- | Map over the 'Gen' inside of 'Jack'.
mapGen :: (Gen (Tree a) -> Gen (Tree b)) -> Jack a -> Jack b
mapGen f =
  Jack . f . runJack

-- | Map over the 'Tree' inside a 'Jack'.
mapTree :: (Tree a -> Tree b) -> Jack a -> Jack b
mapTree =
  mapGen . fmap

-- | Apply an additional shrinker to all generated trees.
reshrink :: (a -> [a]) -> Jack a -> Jack a
reshrink =
  mapTree . expandTree

-- | Flipped version of 'reshrink'.
withShrink :: Jack a -> (a -> [a]) -> Jack a
withShrink =
  flip reshrink
