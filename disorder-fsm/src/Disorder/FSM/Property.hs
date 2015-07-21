{-# LANGUAGE NoImplicitPrelude #-}
module Disorder.FSM.Property (
    module X
  -- * Assert helper functions
  , assert
  , assertMsg
  , (===)
  , (=/=)
  ) where

import           Control.Monad
import           Control.Monad.Catch (MonadThrow(..))

import           Data.Bool
import           Data.Eq
import           Data.Function
import           Data.Monoid
import           Data.String

import           Disorder.FSM.Core

import           Prelude (Show(..))

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
