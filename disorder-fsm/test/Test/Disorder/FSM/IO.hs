{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.FSM.IO where

import           Data.IORef
import           Data.Monoid
import           Control.Applicative hiding (empty)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Control.Monad.Reader
import           Control.Monad.State

import           Disorder.FSM

import           Test.QuickCheck
import           Test.QuickCheck.Monadic


-- | "External" stateful system "Mutable stack" which is tested here
newtype Stack a = Stack {
    _toRef :: IORef [a]
  }

empty :: IO (Stack a)
empty = Stack <$> newIORef []

push :: Stack a -> a -> IO ()
push (Stack r) a = modifyIORef r (a:)

pop :: Stack a -> IO a
pop (Stack r) = readIORef r >>= \case
  [] -> fail "Empty stack"
  (a:ls) -> do
    writeIORef r ls
    return a

top :: Stack a -> IO (Maybe a)
top (Stack r) = readIORef r >>= \case
  [] -> return Nothing
  ls -> return $ Just (head ls)

size :: Stack a -> IO Int
size (Stack r) = readIORef r >>= return . length

-- | Environment here is the 'Stack' itself
type Environment a = Stack a

-- | Model state of the 'Stack' is just a list
type Model a = [a]

type StackTransition a = ReaderT (Environment a) (StateT (Model a) IO)

runFileT :: StackTransition a b -> IO b
runFileT x = do
  e <- empty
  flip evalStateT [] . flip runReaderT e $ x

-- | Pushes random element to stack
genPush :: Gen (Transition (StackTransition Int) ())
genPush = do
  a <- arbitrary
  return . mkTransition ("push " ++ show a) $ do
    s <- lift ask
    liftIO $ push s a
    -- updates the model
    lift . lift . modify $ (a:)

-- | Pops an element from 'Stack'
genPop :: Gen (Transition (StackTransition Int) ())
genPop =
  return . mkTransition "pop" $ do
    s <- lift ask
    l <- lift $ lift get
    pre . not . null $ l
    a <- liftIO $ pop s
    assert (a == head l)
    --  updates the Model
    lift . lift . modify $ tail

-- | Broken 'push' which doesn't actually push negative elements
genInvalidPush :: Gen (Transition (StackTransition Int) ())
genInvalidPush = do
  a <- arbitrary
  return $ mkTransition ("invalid_push " ++ show a) $ do
    s <- lift ask
    -- | this is where it is brocken
    unless (a < 0) $ liftIO $ push s a
    -- | it does modify the model correctly (so it won't match the 'RealWorld')
    lift . lift . modify $ (a:)

-- | Does not specify 'goif' pre-condition so the assertion can fail
--   when it is executed with empty 'Stack'
genInvalidPop :: Gen (Transition (StackTransition Int) ())
genInvalidPop =
  return . mkTransition "invalid_pop" $ do
    s <- lift ask
    l <- lift $ lift get
    -- this may 'fail'
    assert (not . null $ l)
    a <- liftIO $ pop s
    -- and this may 'fail' if the model is out of sync with 'RealWorld'
    assert (a == head l)
    lift . lift . modify $ tail

-- | Does not specify 'goif' pre-condition so it may throw exception
genPopException :: Gen (Transition (StackTransition Int) ())
genPopException =
  return . mkTransition "invalid_pop" $ do
    s <- lift ask
    l <- lift $ lift get
    -- will throw exception on empty 'Stack'
    a <- liftIO $ pop s
    assert (a == head l)
    lift . lift . modify $ tail

-- | Reads an element on 'Stack' top
genTop :: Gen (Transition (StackTransition Int) ())
genTop =
  return $ mkTransition "top" $ do
    s <- lift ask
    ma <- liftIO $ top s
    l <- lift $ lift get
    -- checks with model
    case (ma, l) of
      (Just a, (x:_)) -> assert $ a == x
      (Nothing, _) -> assert $ l == []
      inv -> fail $ "invalid state: " <> show inv

genSize :: Gen (Transition (StackTransition Int) ())
genSize =
  return $ mkTransition "size" $ do
    s <- lift ask
    ss <- liftIO $ size s
    l <- lift $ lift get
    assert $ ss == length l


testIO :: Testable a => IO a -> Property
testIO = monadicIO . (=<<) stop . run

testStack :: PropertyM (StackTransition a) b -> Property
testStack =
   monadic (testIO . runFileT)

-- | Shall never fail since only correct transitions are used
prop_success :: Property
prop_success = testStack $ do
  runFSM . listOf . oneof $ [genPush, genPop, genTop, genSize]

-- | fails due to invalid 'pop'
prop_pop_assert_error :: Property
prop_pop_assert_error = expectFailure . testStack $
  runFSM . listOf . oneof $ [genPush, genInvalidPop, genTop, genSize]

-- | fails due to invalid 'push'
prop_push_assert_error :: Property
prop_push_assert_error = expectFailure . testStack $
  runFSM . listOf . oneof $ [genInvalidPush, genPush, genPop, genTop, genSize]

-- | Produces "invalid" transition less frequently
--   thus generating longer transition list
prop_assert_error_longer_chain :: Property
prop_assert_error_longer_chain = expectFailure . testStack $
  runFSM . vectorOf 100 . frequency $ [
      (10, genPush)
    , (1, genInvalidPush)
    , (10, genTop)
    , (10, genPop)
    , (1, genInvalidPop)
    , (10, genSize)
    ]

-- | Fails due to exception with output identical to 'fail' failure
prop_state_exception :: Property
prop_state_exception =  expectFailure . testStack $
  runFSM . listOf . oneof $ [genPush, genPopException, genTop, genSize]


return []
tests :: IO Bool
tests = $quickCheckAll
