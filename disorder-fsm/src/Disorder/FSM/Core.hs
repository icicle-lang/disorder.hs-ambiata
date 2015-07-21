{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
module Disorder.FSM.Core (
  -- * Transition type and constructor
    Transition(..)
  , mapTransition
  , Action(..)
  , mapAction
  -- * Lift functions for 'Gen'
  , liftGen
  -- * Transition evaluation
  , runFSM
  -- * Transition evaluation
  , mapPropertyM
  , mapContT'
  , pickGen
  ) where

import           Control.Applicative (Applicative)
import           Control.Exception.Base (AssertionFailed(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad (Monad(..))
import           Control.Monad.Catch (MonadCatch(..), MonadThrow(..), catchAll)
import           Control.Monad.Cont (ContT(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (MonadReader(..))
import           Control.Monad.State (MonadState(..), evalStateT, modify)

import           Data.Function ((.), ($), const)
import           Data.Functor (Functor(..))
import           Data.Maybe (Maybe(..))
import           Data.Monoid (Monoid(..), (<>))
import           Data.String (String)

import           Disorder.FSM.Catch

import           Prelude (Show(..))

import           Test.QuickCheck (Property)
import           Test.QuickCheck.Monadic (PropertyM(..))
import           Test.QuickCheck.Gen (Gen(..))


-- | Defines a transition from state to state
data Transition m a = MkTransition {
    -- | Display name (used in case of failure)
    name :: String
    -- | Transition action
  , transition :: Action m a
  }

instance Show (Transition m a) where
  show = name

mapTransition :: (m Property -> n Property) -> (n Property -> m Property) -> Transition m a -> Transition n a
mapTransition f g (MkTransition n a) = MkTransition n (mapAction f g a)


newtype Action m a = Action {
    runAction :: PropertyM m a
  } deriving (
    Functor
  , Applicative
  , MonadIO
  )

instance (MonadThrow m, MonadState s m) => MonadState s (Action m) where
  state = lift . state

instance (MonadThrow m, MonadReader r m) => MonadReader r (Action m) where
  ask = lift ask
  local f (Action m) = Action $ do
    r <- lift $ ask
    mapPropertyM (local f) (local (const r)) m

instance MonadThrow m => Monad (Action m) where
  return = Action . return
  Action m >>= k = Action $ m >>= runAction . k
  fail = lift . throwM . AssertionFailed

instance MonadTrans Action where
  lift = Action . lift

-- | Lift generator to 'Action'
liftGen :: (Monad m, Show a) => Gen a -> Action m a
liftGen = Action . pickGen

mapAction :: (m Property -> n Property) -> (n Property -> m Property) -> Action m a -> Action n a
mapAction f g = Action . mapPropertyM f g . runAction


-- | Main basic function for running FSM models
runFSM :: (MonadState s m, MonadCatch m, MonadThrow m, Show s) => PropertyM m (Maybe (Transition m ())) -> PropertyM m ()
runFSM g =
  runCatchable $ evalStateT go mempty
  where
    go = do
      s <- lift . lift . lift $ get
      modify $ (<> "\n(" <> show s <> ") ")
      ts1 <- get
      mt <- (lift . lift) g `catchAll` handleException ts1
      case mt of
        Nothing -> return ()
        Just t -> do
          modify $ (<> "-> { " <> name t <> " } -> ")
          ts2 <- get
          (lift . lift . runAction . transition) t `catchAll` handleException ts2
          go

    handleException ts e = fail $ ts <> "Exception thrown: " <> show e


mapPropertyM :: (m Property -> n Property) -> (n Property -> m Property) -> PropertyM m a -> PropertyM n a
mapPropertyM f g (MkPropertyM h) = MkPropertyM $ \c -> fmap f (h (fmap g . c))

mapContT' :: ((a -> n r) -> (a -> m r)) -> (m r -> n r) -> ContT r m a -> ContT r n a
mapContT' f g m = ContT $ g . runContT m . f


pickGen :: Gen a -> PropertyM m a
pickGen g = MkPropertyM $ \c ->  g >>= c
