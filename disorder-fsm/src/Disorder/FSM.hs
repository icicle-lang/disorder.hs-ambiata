{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Disorder.FSM (
    module X
  -- * Transition type and constructor
  , Transition
  , Action
  , mkTransition
  , ask
  , get
  , put
  , modify
  -- * Combinators to make 'Transition' less trivial
  , goif
  , goto
  -- * Stubs for combinators arguments
  , always
  , sameState
  -- * Lift functions for 'Gen'
  , liftGen
  -- * Assert helper functions
  , assert
  , assertMsg
  , (===)
  , (=/=)
  -- * Transition evaluation
  , runFSM
  , runFSMCont
  ) where

import           Prelude (Show(..))
import           Data.Bool
import           Data.Eq
import           Data.Function
import           Data.Monoid
import           Data.String
import           Control.Exception.Base (AssertionFailed(..))
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Trans.Cont
import           Control.Monad.RWS.Strict hiding (state)

import           Test.QuickCheck as X hiding ((===))
import           Test.QuickCheck.Monadic as X hiding (assert)

import           Test.QuickCheck.Gen


-- | Defines a transition from state to state
data Transition e s m = MkTransition {
    -- | Display name (used in case of failure)
    name :: String
    -- | Indicates is this transition is applicable for a given environment 'e' and state 's'
  , preCond :: s -> Bool -- Precondition e s m Bool
    -- | Transition action
  , transition :: Action e s m ()
  }

instance Show (Transition e s m) where
  show = name

newtype Action e s m p = Action {
    runAction :: RWST e String s (PropertyM m) p
  } deriving (
    Functor
  , MonadReader e
  , MonadState s
  , MonadIO
  )

instance MonadThrow m => Monad (Action e s m) where
  return = Action . return
  Action m >>= k = Action $ m >>= runAction . k
  fail = lift . throwM . AssertionFailed

instance MonadTrans (Action e s) where
  lift = Action . lift . lift


-- | Constructor for 'Transition'
--   Create unconditional 'Transition' which does nothing
mkTransition :: (MonadThrow m) => String -> Transition e s m
mkTransition name' = MkTransition name' always sameState

-- | Define condition to 'Transition'
goif :: Transition e s m -> (s -> Bool) -> Transition e s m
goif t preCond' = t { preCond = preCond' }

-- | Define transition logic
goto :: Transition e s m -> Action e s m () -> Transition e s m
goto t transition' = t { transition = transition' }

-- | Do not change the state or eval any monadic actions
sameState :: MonadThrow m => Action e s m ()
sameState = return ()

-- | Make 'Transition' unconditional
always :: s -> Bool
always = const True

-- | Lift generator to 'Action'
liftGen :: (Monad m, Show a) => Gen a -> Action e s m a
liftGen g = Action . lift . pickGen $ g

-- | FSM implementation of QC 'assert'
assert :: (MonadThrow m) => Bool -> Action e s m ()
assert = flip assertMsg "Assertion failed"

-- | Like 'assert' but with custom message displayed in case of failure
assertMsg :: (MonadThrow m) => Bool -> String -> Action e s m ()
assertMsg p msg =
  unless p $ fail msg

-- | FSM implementation of QC '(===)'
infix 4 ===
(===) :: (Eq a, Show a, Monad m, MonadThrow m) => a -> a -> Action e s m ()
x === y = (x == y) `assertMsg` (show x <> " /= " <> show y)

infix 4 =/=
(=/=) :: (Eq a, Show a, Monad m, MonadThrow m) => a -> a -> Action e s m ()
x =/= y = (x /= y) `assertMsg` (show x <> " == " <> show y)


-- | Similar to 'runFSM' but can be used with 'ContT' monad
--   'ContT' does not define 'MonadCatch' instance, so it it cannot be passed to 'runFSM'
runFSMCont :: (MonadCatch m, Show s) => r -> s -> Gen (Transition r s (ContT Property m)) -> PropertyM (ContT Property m) ()
runFSMCont env state g = do
  Positive n <- pickGen arbitrary
  void $ evalRWST (replicateM_ n go) env (state, mempty)
  where
    go = do
      (s, ts) <- get
      t <- lift . pickGen $ g `suchThat` flip preCond s
      put $ (s, ts <> formatState t s)
      let m = RWST $ \r (s', ss') -> do
           (a'', s'', w'') <- runRWST (runAction (transition t)) r s'
           return (a'', (s'', ss'), w'')
      m `handleRun` handleAssert `handleRun` handleException

    handleRun (RWST m) h = RWST $ \r s@(_, w) ->
      m r s `catchPropCont` \e -> runRWST (h e w) r s

    handleAssert (e :: AssertionFailed) s = formatFail "Assertion failed" s e
    handleException (e :: SomeException) s = formatFail "Exception thrown" s e
    formatFail m s e = fail $ s <> m <> ": " <> show e

    formatState t s = "\n(" <> show s <> ") -> " <> name t <> " -> "

-- | Generate and execute a list of 'Transition's
--   given environment 'env' and initial state 'state'
runFSM :: (MonadCatch m, Show s) => e -> s -> Gen (Transition e s m) -> PropertyM m ()
runFSM env state g =
  mapPropertyM fromCont toCont $
    runFSMCont env state $ do
      MkTransition n p a <- g
      return $ MkTransition n p $
          Action $ mapRWST (mapPropertyM toCont fromCont) . runAction $ a
  where
    toCont = lift
    fromCont = evalContT


catchPropCont :: (Exception e, MonadCatch m) => PropertyM (ContT Property m) a -> (e -> PropertyM (ContT Property m) a) -> PropertyM (ContT Property m) a
catchPropCont (MkPropertyM f) g = MkPropertyM $ \h -> catchGenCont (f h) (\e -> unPropertyM (g e) h)

catchGenCont :: (Exception e, MonadCatch m) => Gen (ContT r m r) -> (e -> Gen (ContT r m a)) -> Gen (ContT r m a)
catchGenCont (MkGen f) g = MkGen $ \q i -> catchCont (f q i) (\e -> unGen (g e) q i)

catchCont :: (Exception e, MonadCatch m) => ContT r m r -> (e -> ContT r m a) -> ContT r m a
catchCont m g = ContT $ \c -> (runContT m return `catch` (\e -> runContT (g e) c)) -- >>= k

mapPropertyM :: (m Property -> n Property) -> (n Property -> m Property) -> PropertyM m a -> PropertyM n a
mapPropertyM f g (MkPropertyM h) = MkPropertyM $ \c -> fmap f (h (fmap g . c))

pickGen :: Gen a -> PropertyM m a
pickGen g = MkPropertyM $ \c ->  g >>= c
