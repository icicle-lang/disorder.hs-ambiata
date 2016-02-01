{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.FSM.IO where

import           Control.Applicative hiding (empty)
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.IORef
import           Data.Monoid

import           Disorder.FSM

import           Prelude


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

type StackTransition a = ConditionalTransition (Environment a) (Model a) IO

-- | Pushes random element to stack
genPush :: Gen (StackTransition Int)
genPush = do
  a <- arbitrary
  return $ mkTransition ("push " ++ show a) `goto` do
    s <- ask
    liftIO $ push s a
    -- updates the model
    modify $ (a:)

-- | Broken 'push' which doesn't actually push negative elements
genInvalidPush :: Gen (StackTransition Int)
genInvalidPush = do
  a <- arbitrary
  return $ mkTransition ("invalid_push " ++ show a) `goto` do
    s <- ask
    -- | this is where it is broken
    unless (a < 0) $ liftIO $ push s a
    -- | it does modify the model correctly (so it won't match the 'RealWorld')
    modify $ (a:)

genSlowPush :: Gen (StackTransition Int)
genSlowPush = do
  a <- arbitrary
  return $ mkTransition ("push " ++ show a) `goto` do
    s <- ask
    liftIO $ do
      threadDelay $ 100*1000 -- 0.1 sec
      push s a
    -- updates the model
    modify $ (a:)

-- | Pops an element from 'Stack'
genPop :: Gen (StackTransition Int)
genPop =
  return $ mkTransition "pop" `goif` (not . null) `goto` do
    s <- ask
    a <- liftIO $ pop s
    l <- get
    -- popped element must match head element in model
    a === head l
    -- updates the Model
    modify $ tail

-- | Does not specify 'goif' pre-condition so the assertion can fail
--   when it is executed with empty 'Stack'
genInvalidPop :: Gen (StackTransition Int)
genInvalidPop =
  return $ mkTransition "invalid_pop" `goto` do
    s <- ask
    l <- get
    -- this may 'fail'
    void $ assertMsg (l /= []) "pop on empty stack"
    a <- liftIO $ pop s
    -- and this may 'fail' if the model is out of sync with 'RealWorld'
    a === head l
    modify $ tail

-- | Does not specify 'goif' pre-condition so it may throw exception
genPopException :: Gen (StackTransition Int)
genPopException =
  return $ mkTransition "invalid_pop" `goto` do
    s <- ask
    l <- get
    -- will throw exception on empty 'Stack'
    a <- liftIO $ pop s
    a === head l
    modify $ tail

-- | Reads an element on 'Stack' top
genTop :: Gen (StackTransition Int)
genTop =
  return $ mkTransition "top" `goto` do
    s <- ask
    ma <- liftIO $ top s
    l <- get
    -- checks with model
    case (ma, l) of
      (Just a, (x:_)) -> a === x
      (Nothing, _) -> assertMsg (l == []) "l is not empty"
      inv -> fail $ "invalid state: " <> show inv

genSize :: Gen (StackTransition Int)
genSize =
  return $ mkTransition "size" `goto` do
    s <- ask
    ss <- liftIO $ size s
    l <- get
    ss === length l

genFailIf :: (Model Int -> Bool) -> Gen (StackTransition Int)
genFailIf p =
  return $ mkTransition "failIf" `goif` p `goto` fail "failIf p == True"


-- | Shall never fail since only correct transitions are used
prop_success :: Property
prop_success = monadicIO $ do
  s <- liftIO empty
  runFSMGen s [] $ shuffle [genPush, genPop, genTop, genSize]

prop_test :: Property
prop_test = monadicIO $ do
  s <- liftIO empty
  runFSMGen s [] $ shuffle [genTop]

-- | fails due to invalid 'pop'
prop_pop_assert_error :: Property
prop_pop_assert_error = expectFailure . monadicIO $ do
  s <- liftIO empty
  runFSMGen s [] $ shuffle [genPush, genInvalidPop, genTop, genSize]

-- | fails due to invalid 'push'
prop_push_assert_error :: Property
prop_push_assert_error = expectFailure . monadicIO $ do
  s <- liftIO empty
  runFSMGen s [] $ shuffle [genInvalidPush, genPush, genPop, genTop, genSize]

-- | Produces "invalid" transition less frequently
--   thus generating longer transition list
prop_assert_error_longer_chain :: Property
prop_assert_error_longer_chain = expectFailure . monadicIO $ do
  s <- liftIO empty
  runFSMUntil 100 s [] . frequencyShuffle $ [
      (10, genPush)
    , (1, genInvalidPush)
    , (10, genTop)
    , (10, genPop)
    , (1, genInvalidPop)
    , (10, genSize)
    ]

-- | Fails due to exception with output identical to 'fail' failure
prop_state_exception :: Property
prop_state_exception =  expectFailure . monadicIO $ do
  s <- liftIO empty
  runFSMGen s [] $ shuffle  [genPush, genPopException, genTop, genSize]


prop_limitedByCount :: Property
prop_limitedByCount = once . monadicIO $ do
  s <- liftIO empty
  runFSMUntil 10 s [] $ shuffle  [genPush, genFailIf ((>10) . length)]


prop_limitedByTime :: Property
prop_limitedByTime = once . monadicIO $ do
  s <- liftIO empty
  -- Execution is limited by 1 second
  runFSMFor (fromRational 1) s [] $ shuffle  [genSlowPush, genFailIf ((>10) . length)]


-- Doesn't look like it is possible to specify in QC
-- that the test with 100 "Give up" results is a success
-- commenting it out
-- | Check that FSM testing doesn't stuck if there is no valid transitions available
-- prop_no_valid_transition :: Property
-- prop_no_valid_transition = once . expectFailure . monadicIO $ do
--   s <- liftIO empty
--   runFSMUntil 10 s [] $ pure [genPop]


return []
tests :: IO Bool
tests = $quickCheckAll
