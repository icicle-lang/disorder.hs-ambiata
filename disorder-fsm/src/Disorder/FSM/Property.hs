{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Disorder.FSM.Property (
    module X
  -- * Assert helper functions
  , assert
  , assertMsg
  , (===)
  , (=/=)
  -- * Biased 'shuffle' with 'frequency'-like coefficients
  , frequencyShuffle
  ) where

import           Control.Monad
import           Control.Monad.Catch (MonadThrow(..))

import           Data.Bool
import           Data.Eq
import           Data.Function
import           Data.List (foldl')
import           Data.Monoid
import           Data.Ord
import           Data.String

import           Disorder.FSM.Core

import           Prelude (Int, Show(..), Num(..), error)

import           Test.QuickCheck as X hiding ((===))
import           Test.QuickCheck.Monadic as X hiding (assert)


-- | FSM implementation of QC 'assert'
assert :: (MonadThrow m) => Bool -> Action m ()
assert = flip assertMsg "Assertion failed"

-- | Like 'assert' but with custom message displayed in case of failure
assertMsg :: (MonadThrow m) => Bool -> String -> Action m ()
assertMsg p msg =
  unless p $ fail msg

-- | FSM implementation of QC '(===)'
infix 4 ===
(===) :: (Eq a, Show a, Monad m, MonadThrow m) => a -> a -> Action m ()
x === y = (x == y) `assertMsg` (show x <> " /= " <> show y)

infix 4 =/=
(=/=) :: (Eq a, Show a, Monad m, MonadThrow m) => a -> a -> Action m ()
x =/= y = (x /= y) `assertMsg` (show x <> " == " <> show y)


frequencyShuffle :: [(Int, a)] -> Gen [a]
frequencyShuffle xs =
  let s = foldl' (\s' (k,_) -> s'+k) 0 xs
  in go s xs
  where
    go _ [] = return []
    go s kxs = do
      n <- choose(1,s)
      let ((k,x):kxs') = extract n kxs
      xs' <- go (s-k) kxs'
      return $ x : xs'
    extract n kxs@((k,_):_) | n <= k = kxs
    extract n (kx@(k,_):kxs) =
      let (kx':kxs') = extract (n-k) kxs
      in kx' : kx : kxs'
    extract _ [] = error "Invalid input"
