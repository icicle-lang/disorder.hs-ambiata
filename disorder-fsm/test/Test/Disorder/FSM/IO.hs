{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.FSM.IO where

import           Data.IORef
import           Data.Monoid
import           Control.Applicative hiding (empty)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Disorder.FSM

import           Test.QuickCheck
import           Test.QuickCheck.Monadic


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


type StackTransition a = Transition (Stack a) [a] IO

genPush :: Gen (StackTransition Int)
genPush = do
  a <- arbitrary
  return $ mkTransition ("push " ++ show a) `goto` do
    s <- ask
    liftIO $ push s a
    modify $ (a:)

-- | Broken 'push' which doesn't actually push negative elements
genInvalidPush :: Gen (StackTransition Int)
genInvalidPush = do
  a <- arbitrary
  return $ mkTransition ("invalid_push " ++ show a) `goto` do
    s <- ask
    unless (a < 0) $ liftIO $ push s a
    modify $ (a:)


genPop :: Gen (StackTransition Int)
genPop =
  return $ mkTransition "pop" `goif` (not . null) `goto` do
    s <- ask
    a <- liftIO $ pop s
    l <- get
    lift $ assert (a == head l)
    modify $ tail

-- | Does not specify preCondition so the assertion can fail
genInvalidPop :: Gen (StackTransition Int)
genInvalidPop =
  return $ mkTransition "invalid_pop" `goto` do
    s <- ask
    l <- get
    lift $ assert (not . null $ l)
    a <- liftIO $ pop s
    lift $ assert (a == head l)
    modify $ tail

-- | Does not specify preCondition so it will throw exception
genPopException :: Gen (StackTransition Int)
genPopException =
  return $ mkTransition "invalid_pop" `goto` do
    s <- ask
    l <- get
    a <- liftIO $ pop s
    lift $ assert (a == head l)
    modify $ tail

genTop :: Gen (StackTransition Int)
genTop =
  return $ mkTransition "top" `goto` do
    s <- ask
    ma <- liftIO $ top s
    l <- get
    case (ma, l) of
      (Just a, (x:_)) -> lift $ assert $ a == x
      (Nothing, _) -> lift $ assert $ l == []
      inv -> fail $ "invalid state: " <> show inv

genSize :: Gen (StackTransition Int)
genSize =
  return $ mkTransition "size" `goto` do
    s <- ask
    ss <- liftIO $ size s
    l <- get
    lift . assert $ ss == length l

prop_success :: Property
prop_success = monadicIO $ do
  s <- liftIO empty
  runFSM s [] (listOf $ oneof [genPush, genPop, genTop, genSize])

prop_pop_assert_error :: Property
prop_pop_assert_error = expectFailure . monadicIO $ do
  s <- liftIO empty
  runFSM s [] . listOf . oneof $ [genPush, genInvalidPop, genTop, genSize]

prop_push_assert_error :: Property
prop_push_assert_error = expectFailure . monadicIO $ do
  s <- liftIO empty
  runFSM s [] . listOf . oneof $ [genInvalidPush, genPush, genPop, genTop, genSize]


-- | Produces "invalid" transition less frequently
--   thus generating longer transition list
prop_assert_error_longer_chain :: Property
prop_assert_error_longer_chain = expectFailure . monadicIO $ do
  s <- liftIO empty
  runFSM s [] . vectorOf 100 . frequency $ [
      (10, genPush)
    , (1, genInvalidPush)
    , (10, genTop)
    , (10, genPop)
    , (1, genInvalidPop)
    , (10, genSize)
    ]

prop_state_exception :: Property
prop_state_exception =  expectFailure . monadicIO $ do
  s <- liftIO empty
  runFSM s [] . listOf . oneof $ [genPush, genPopException, genTop, genSize]


return []
tests :: IO Bool
tests = $quickCheckAll
