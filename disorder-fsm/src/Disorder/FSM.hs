{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Disorder.FSM (
  -- * Transition type and constructor
    Transition
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
  -- * Transition evaluation
  , runFSM
  , runFSMCont
  ) where

import           Prelude (Show(..))
import           Data.Bool
import           Data.String
import           Data.Maybe
import           Data.Tuple
import           Data.Function
import           Data.Monoid
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Reader (MonadReader(..))
import           Control.Monad.RWS.Strict (RWST(..), evalRWST)
import           Control.Monad.State (MonadState(get, put), modify)

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
  , transition :: TransitionAction e s m
  }

newtype Action e s m a = Action {
    runAction :: RWST e () s m a
  } deriving (
    Functor
  , Monad
  , MonadReader e
  , MonadState s
  , MonadTrans
  , MonadIO
  , MonadThrow
  )

evalAction :: Monad m => Action e s m a -> e -> s -> m a
evalAction m e s = return . fst =<< evalRWST (runAction m) e s

type TransitionAction e s m = Action e s (PropertyM m) ()

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
goto :: Transition e s m -> (TransitionAction e s m) -> Transition e s m
goto t transition' = t { transition = transition' }

-- | Do not change the state or eval any monadic actions
sameState :: Monad m => TransitionAction e s m
sameState = return ()

-- | Make 'Transition' unconditional
always :: s -> Bool
always = const True


-- | Generate and execute a list of 'Transition's
--   given environment 'env' and initial state 'state'
--   For longer running 'Transition's limit the list produced by 'g'
runFSM :: (Show s, Monad m, MonadCatch m) => e -> s -> Gen [Transition e s m] -> PropertyM m ()
runFSM env state g = forAllM g $ \ts ->
  let m = sequence_ . fmap toTransition $ ts
  in evalAction m env state
  where
    toTransition t = do
      s <- get
      lift . pre $ preCond t s -- discard the transition if preCond t s == False
      lift $ handleError t s -- add transition chain to QuickCheck error in case of failure
      transition t `catchTran` handleException -- run transition with additional handling

    handleException :: Monad m => SomeException -> TransitionAction e s m
    handleException e = fail $ "Exception thrown: " <> show e

-- | Similar to 'runFSM' but can be used with 'ContT' monad
--   'ContT' does not define 'MonadCatch' instance, so it it cannot be passed to 'runFSM'
runFSMCont :: (Show s, Monad m, MonadCatch m) => e -> s -> Gen [Transition e s (ContT Property m)] -> PropertyM (ContT Property m) ()
runFSMCont env state g = forAllM g $ \ts ->
  let m = sequence_ . fmap toTransition $ ts
  in evalAction m env (state, "")
  where
   toTransition t = do
     (s, _) <- get
     lift . pre $ preCond t s -- discard the transition if preCond t s == False
     lift $ handleError t s -- add transition chain to QuickCheck error in case of failure
     modify $ \(s', ss') -> (s', ss' <> formatState t s)
     let m = Action . RWST $ \r (s', ss') -> do
           (a'', s'', w'') <- runRWST (runAction (transition t)) r s'
           return (a'', (s'', ss'), w'')
     m `catchTranCont` handleException

   handleException :: SomeException -> TransitionAction r (s, String) (ContT Property m)
   handleException e = do
     (_, ss) <- get
     fail $ ss <> "Exception thrown: " <> show e


handleError :: (Show a, Monad m) => Transition e s m1 -> a -> PropertyM m ()
handleError t s = monitor (mapTotalResult (addFailureReason t s))

addFailureReason :: Show a => Transition e s m -> a -> Result -> Result
addFailureReason t s = \case
 r@MkResult{ ok = Just False } -> r { reason = formatState t s <> reason r}
 r -> r

formatState :: Show a => Transition e s m -> a -> String
formatState t s = "\n(" <> show s <> ") -> " <> name t <> " -> "



-- | There is no MonadCatch instance for PropertyM so here is how it could be implemented
catchTran :: (Exception e, MonadCatch m) => TransitionAction r s m -> (e -> TransitionAction r s m) -> TransitionAction r s m
catchTran (Action (RWST m)) g = Action . RWST $ \r s -> m r s `catchProp` \e -> (runRWST . runAction) (g e) r s

catchProp :: (Exception e, MonadCatch m) => PropertyM m a -> (e -> PropertyM m a) -> PropertyM m a
catchProp (MkPropertyM f) g = MkPropertyM $ \h -> f h `catchGen` \e -> unPropertyM (g e) h

catchGen :: (Exception e, MonadCatch m) => Gen (m a) -> (e -> Gen (m a)) -> Gen (m a)
catchGen (MkGen f) g = MkGen $ \q i -> f q i `catch` \e -> unGen (g e) q i


catchTranCont :: (Exception e, MonadCatch m) =>
  Action r s (PropertyM (ContT Property m)) a ->
  (e -> Action r s (PropertyM (ContT Property m)) a)
  -> Action r s (PropertyM (ContT Property m)) a
catchTranCont (Action (RWST m)) g = Action . RWST $ \r s -> m r s `catchPropCont` \e -> (runRWST . runAction) (g e) r s

catchPropCont :: (Exception e, MonadCatch m) =>
  PropertyM (ContT Property m) a -> (e -> PropertyM (ContT Property m) a) -> PropertyM (ContT Property m) a
catchPropCont (MkPropertyM f) g = MkPropertyM $ \h -> f h `catchGenCont` \e -> unPropertyM (g e) h

catchGenCont :: (Exception e, MonadCatch m) => Gen (ContT r m a) -> (e -> Gen (ContT r m r)) -> Gen (ContT r m a)
catchGenCont (MkGen f) g = MkGen $ \q i -> f q i `catchCont` \e -> unGen (g e) q i

catchCont :: (Exception e, MonadCatch m) => ContT r m a -> (e -> ContT r m r) -> ContT r m a
catchCont m g = mapContT (`catch` ((`runContT`return) . g)) m
