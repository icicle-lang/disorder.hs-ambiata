{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.FSM.Catch (
    CatchableT(..)
  , CatchableContT
  , runCatchableContT
  , evalCatchableContT
  ) where

import           Control.Applicative (Applicative)
import           Control.Monad (Monad(..))
import           Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import           Control.Monad.Cont (ContT(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (MonadReader(..))
import           Control.Monad.State (MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans(..))

import           Data.Function ((.), ($), flip)
import           Data.Functor (Functor(..))

import           Test.QuickCheck.Gen (Gen(..))
import           Test.QuickCheck.Monadic (PropertyM(..))


newtype CatchableT m a = CatchableT {
    runCatchable :: m a
  } deriving (
    Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader e
  , MonadState s
  )

instance MonadTrans CatchableT where
  lift = CatchableT

instance MonadThrow m => MonadThrow (CatchableT (PropertyM m)) where
  throwM = CatchableT . lift . throwM

instance (MonadCatch m, MonadThrow m) => MonadCatch (CatchableT (PropertyM m)) where
  catch (CatchableT (MkPropertyM f)) g = CatchableT . MkPropertyM $ \h -> catchGen (f h) (\e -> unPropertyM (runCatchable (g e)) h)
    where
      catchGen (MkGen f') g' = MkGen $ \q i -> f' q i `catch` (\e -> unGen (g' e) q i)

instance MonadThrow m => MonadThrow (CatchableT (ContT r m)) where
  throwM = CatchableT . throwM

instance (MonadCatch m, MonadThrow m) => MonadCatch (CatchableT (ContT r m)) where
  catch (CatchableT m) g = CatchableT . ContT $ \c -> runContT m c `catch` (\e -> runContT (runCatchable (g e)) c)


type CatchableContT r m = CatchableT (ContT r m)

runCatchableContT :: CatchableContT r m a -> (a -> m r) -> m r
runCatchableContT = runContT . runCatchable

evalCatchableContT :: Monad m => CatchableContT r m r -> m r
evalCatchableContT = flip runContT return . runCatchable
