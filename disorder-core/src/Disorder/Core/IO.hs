module Disorder.Core.IO (
    testM
  , testPropertyM
  , testIO
  , testPropertyIO
  , withCPUTime
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import           Test.QuickCheck (Testable, Property, ioProperty)
import           Test.QuickCheck.Monadic (PropertyM, monadic, run, stop)

import           System.CPUTime (getCPUTime)


testM :: (Monad m, Testable a) => (m Property -> Property) -> m a -> Property
testM prop =
  testPropertyM prop . run

testPropertyM :: (Monad m, Testable a) => (m Property -> Property) -> PropertyM m a -> Property
testPropertyM prop =
  monadic prop . (=<<) stop

testIO :: Testable a => IO a -> Property
testIO =
  testM ioProperty

testPropertyIO :: Testable a => PropertyM IO a -> Property
testPropertyIO =
  testPropertyM ioProperty

-- | Perform an action and return the CPU time it takes, in picoseconds
-- (actual precision varies with implementation).
withCPUTime :: MonadIO m => m a -> m (Integer, a)
withCPUTime a = do
  t1 <- liftIO getCPUTime
  r <- a
  t2 <- liftIO getCPUTime
  return (t2 - t1, r)
