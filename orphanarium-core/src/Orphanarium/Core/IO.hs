module Orphanarium.Core.IO (
    testIO
  ) where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic


testIO :: Testable a => IO a -> Property
testIO = monadicIO . (=<<) stop . run
