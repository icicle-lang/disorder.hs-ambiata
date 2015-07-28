{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Disorder.FSM (
  -- * Transition type and constructor
    Transition
  , mkTransition
  -- * Combinators to make 'Transition' less trivial
  , goif
  , goto
  -- * Stubs for combinators arguments
  , always
  , sameState
  -- * Transition evaluation
  , runFSM
  ) where

import           Prelude (Show(..))
import           Data.Bool
import           Data.String
import           Data.Maybe
import           Data.Function
import           Data.Monoid
import           Control.Monad
import           Control.Monad.Catch

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Property


-- | Defines a transition from state to state
data Transition e s m = MkTransition {
    -- | Display name (used in case of failure)
    name :: String
    -- | Predicate which determines of this transition is applicable for a given state 's'
  , preCond :: s -> Bool
    -- | Transition from state 's' given environment 'e'
    --   Assertion logic can be placed in 'PropertyM m s'
  , transition :: e -> s -> PropertyM m s
  }

instance Show (Transition e s m) where
  show = name

-- | Constructor for 'Transition'
--   Create unconditional 'Transition' which does nothing
mkTransition :: Monad m => String -> Transition e s m
mkTransition name' = MkTransition name' always sameState

-- | Define condition to 'Transition'
goif :: Transition e s m -> (s -> Bool) -> Transition e s m
goif t preCond' = t { preCond = preCond' }

-- | Define transition logic
goto :: Transition e s m -> (e -> s -> PropertyM m s) -> Transition e s m
goto t transition' = t { transition = transition' }

-- | Do not change the state or eval any monadic actions
sameState :: Monad m => e -> s -> PropertyM m s
sameState _ s = return s

-- | Make 'Transition' unconditional
always :: s -> Bool
always = const True


-- | Generate and execute a list of 'Transition's
--   given environment 'env' and initial state 'state'
--   For longer running 'Transition's limit the list produced by 'g'
runFSM :: (Show s, Monad m, MonadCatch m) => e -> s -> Gen [Transition e s m] -> PropertyM m s
runFSM env state g = forAllM g $ foldM runTransition state
  where
    runTransition s t = do
      pre (preCond t s) -- discard the transition if preCond t s == False
      handleError t s -- add transition chain to QuickCheck error in case of failure
      transition t env s `catchProp` handleException -- catch all exceptions converting them into failure

    handleError t s = monitor (mapTotalResult (addFailureReason t s))

    handleException :: Monad m => SomeException -> PropertyM m s
    handleException e = fail $ "Exception thrown: " <> show e

    addFailureReason t s = \case
      r@MkResult{ ok = Just False } -> r { reason = formatState t s <> reason r}
      r -> r

    formatState t s = "\n(" <> show s <> ") -> " <> name t <> " -> "


-- | There is no MonadCatch instance for PropertyM so here is how it could be implemented
catchProp :: (Exception e, MonadCatch m) => PropertyM m a -> (e -> PropertyM m a) -> PropertyM m a
catchProp (MkPropertyM f) g = MkPropertyM $ \h -> f h `catchGen` \e -> unPropertyM (g e) h

catchGen :: (Exception e, MonadCatch m) => Gen (m a) -> (e -> Gen (m a)) -> Gen (m a)
catchGen (MkGen f) g = MkGen $ \q i -> f q i `catch` \e -> unGen (g e) q i
