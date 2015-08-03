{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Disorder.FSM (
    Transition (..)
  , mkTransition
  , runFSM
  , runFSM'
  ) where

import           Prelude (Show(..))

import           Data.String
import           Data.Foldable
import           Data.Function
import           Data.Monoid

import           Control.Monad
import           Control.Monad.Catch

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Property



data Transition m a = Transition {
    name :: String
  , transition :: PropertyM m a
  }

-- Required so that we can use 'forAllM'
instance Show (Transition m a) where
  show = name


mkTransition :: String -> PropertyM m a -> Transition m a
mkTransition = Transition

runFSM :: (Monad m, MonadCatch m) => Gen [Transition m a] -> PropertyM m ()
runFSM =
  runFSM' . fmap (fmap (\(Transition n t) -> Transition n $ catchProp t handleException))
  where
    handleException :: Monad m => SomeException -> PropertyM m s
    handleException e = fail $ "Exception thrown: " <> show e

-- The non MonadCatch verison of 'runFSM'
runFSM' :: Monad m => Gen [Transition m a] -> PropertyM m ()
runFSM' g = forAllM g $ \ts ->
  traverse_ toTransition ts
  where
    toTransition t = do
      monitor (counterexample (name t))
      void $ transition t


catchProp :: (Exception e, MonadCatch m) => PropertyM m a -> (e -> PropertyM m a) -> PropertyM m a
catchProp (MkPropertyM f) g = MkPropertyM $ \h -> f h `catchGen` \e -> unPropertyM (g e) h

catchGen :: (Exception e, MonadCatch m) => Gen (m a) -> (e -> Gen (m a)) -> Gen (m a)
catchGen (MkGen f) g = MkGen $ \q i -> f q i `catch` \e -> unGen (g e) q i
