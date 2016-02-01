{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Disorder.FSM.Runner (
  -- * Transition type and constructor
    ConditionalTransition(..)
  , RunnerT
  , mkTransition
  -- * Combinators to make 'Transition' less trivial
  , goif
  , goto
  -- * Stubs for combinators arguments
  , always
  , sameState
  -- *
  , runFSMGen
  , runFSMUntil
  , runFSMFor
  -- *
  , limitBy
  ) where

import           Control.Applicative (Applicative)
import           Control.Monad (Monad(..), mfilter, (<=<))
import           Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (MonadReader(..), ReaderT(..))
import           Control.Monad.RWS (RWST(..), evalRWST)
import           Control.Monad.State (MonadState(..), StateT, evalStateT, modify)
import           Control.Monad.Trans.Class (MonadTrans(..))

import           Data.Bool (Bool(..))
import           Data.Function (($), (.), flip, const)
import           Data.Functor (Functor(..))
import           Data.Maybe (Maybe(..))
import           Data.Ord ((>=), (<))
import           Data.String (String)
import           Data.Time (NominalDiffTime, UTCTime, getCurrentTime, diffUTCTime)
import           Data.Tuple (fst, uncurry)

import           Disorder.FSM.Core

import           Prelude (Enum(..), Int, Show(..))

import           Test.QuickCheck (Gen, Property, arbitrary, suchThatMaybe)
import           Test.QuickCheck.Property (rejected)
import           Test.QuickCheck.Monadic (PropertyM(..), stop)


data ConditionalTransition e s m = ConditionalTransition {
    preCond :: e -> s -> Bool
  , toTransition :: Transition (RunnerT e s m) ()
  }

-- | Constructor for 'ConditionalTransition'
--   Create unconditional 'Transition' which does nothing
mkTransition :: MonadThrow m => String -> ConditionalTransition e s m
mkTransition name' =
  ConditionalTransition always $
    MkTransition name' sameState

-- | Define condition to 'Transition'
goif :: ConditionalTransition e s m -> (s -> Bool) -> ConditionalTransition e s m
goif ct preCond' = ct { preCond = \_ -> preCond' }

-- | Define transition logic
goto :: ConditionalTransition e s m -> Action (RunnerT e s m) () -> ConditionalTransition e s m
goto ct@(ConditionalTransition _ t) transition' =
  ct { toTransition = t { transition = transition' }}

-- | Do not change the state or eval any monadic actions
sameState :: MonadThrow m => Action m ()
sameState = return ()

-- | Make 'Transition' unconditional
always :: e -> s -> Bool
always = const . const True

newtype RunnerT e s m a = RunnerT {
    toRWST :: RWST e () s m a
  } deriving (
    Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader e
  , MonadState s
  , MonadThrow
  , MonadCatch
  , MonadTrans
  )

runRunnerT :: Monad m => e -> s -> RunnerT e s m a -> m a
runRunnerT e s = return . fst <=< (\m -> evalRWST m e s) . toRWST


runFSMGen :: (MonadCatch m, Show s) => e -> s -> Gen [Gen (ConditionalTransition e s m)] -> PropertyM m ()
runFSMGen e s gs = do
  n <- pickGen arbitrary :: PropertyM m Int
  runFSMUntil n e s gs


runFSMUntil :: (MonadCatch m, Show s) => Int -> e -> s -> Gen [Gen (ConditionalTransition e s m)] -> PropertyM m ()
runFSMUntil n e s =
  runFSMWith
    ((runRunnerT e s) . (runCounter n))
    (lift . lift)
    . limitByCount . fromStatefulTransition

runFSMFor :: (MonadIO m, MonadCatch m, Show s) => NominalDiffTime -> e -> s -> Gen [Gen (ConditionalTransition e s m)] -> PropertyM m ()
runFSMFor d e s gs = do
  t0 <- liftIO $ getCurrentTime
  runFSMWith
    ((runRunnerT e s) . (runTimer t0 d))
    (lift . lift)
    . limitByTime . fromStatefulTransition $ gs


runFSMWith :: (MonadState s m, MonadCatch m, Show s)
  => (m Property -> n Property) -> (n Property -> m Property) -> PropertyM m (Maybe (Transition m ()))
  -> PropertyM n ()
runFSMWith l u =
  mapPropertyM l u . runFSM


fromStatefulTransition :: Monad m => Gen [Gen (ConditionalTransition e s m)] -> PropertyM (RunnerT e s m) (Maybe (Transition (RunnerT e s m) ()))
fromStatefulTransition ggs = do
  e <- lift ask
  s <- lift get
  gs <- pickGen $ ggs
  let go = \case
        [] -> stop rejected
        (g:gs') -> do
          mt <- pickGen $ g `suchThatMaybe` \t -> preCond t e s
          case mt of
            Nothing -> go gs'
            Just t -> return . Just . toTransition $ t
  go gs



limitByCount :: Monad m
  => PropertyM m (Maybe (Transition m ()))
  -> PropertyM (CounterT m) (Maybe (Transition (CounterT m) ()))
limitByCount =
  limitBy
    (tick >>= return . mfilter (>= 0) . Just)
    (const lift)
    runCounter

newtype CounterT m a = CounterT {
    toState :: StateT Int m a
  } deriving (
    Applicative
  , Functor
  , Monad
  )

instance (MonadState s m) => MonadState s (CounterT m) where
  state = lift . state

deriving instance (Monad m, MonadCatch (StateT Int m)) => MonadCatch (CounterT m)

deriving instance (Monad m, MonadThrow (StateT Int m)) => MonadThrow (CounterT m)

runCounter :: Monad m => Int -> CounterT m a -> m a
runCounter n = flip evalStateT n . toState

tick :: Monad m => CounterT m Int
tick = CounterT $ modify pred >> get

instance MonadTrans CounterT where
  lift = CounterT . lift


limitByTime :: (Monad m, MonadIO m)
  => PropertyM m (Maybe (Transition m ()))
  -> PropertyM (TimerT m) (Maybe (Transition (TimerT m) ()))
limitByTime =
  limitBy
   tock
   (const lift)
   (uncurry runTimer)

newtype TimerT m a = TimerT {
    toReader :: ReaderT (UTCTime, NominalDiffTime) m a
  } deriving (
    Applicative
  , Functor
  , Monad
  , MonadIO
  )

instance (MonadState s m) => MonadState s (TimerT m) where
  state = lift . state

deriving instance (Monad m, MonadCatch (ReaderT (UTCTime, NominalDiffTime) m)) => MonadCatch (TimerT m)

deriving instance (Monad m, MonadThrow (ReaderT (UTCTime, NominalDiffTime) m)) => MonadThrow (TimerT m)

runTimer :: Monad m => UTCTime -> NominalDiffTime -> TimerT m a -> m a
runTimer t d = flip runReaderT (t,d) . toReader

instance MonadTrans TimerT where
  lift = TimerT . lift

tock :: MonadIO m => TimerT m (Maybe (UTCTime, NominalDiffTime))
tock = do
  t <- liftIO getCurrentTime
  (t0, d) <- TimerT ask
  return $ if diffUTCTime t t0 < d then (Just (t0, d)) else Nothing


limitBy :: (MonadTrans t, Monad m, Monad (t m))
  => t m (Maybe v) -> (v -> m Property -> t m Property) -> (v -> t m Property -> m Property) -> PropertyM m (Maybe (Transition m ()))
  -> PropertyM (t m) (Maybe (Transition (t m) ()))
limitBy mp fu fd g = do
  mv <- lift mp
  case mv of
    Nothing ->
      return Nothing
    Just v -> do
      (fmap . fmap) (\(MkTransition n t) -> MkTransition n (Action . mapPropertyM (fu v) (fd v) . runAction $ t))
        . mapPropertyM (fu v) (fd v) $ g
