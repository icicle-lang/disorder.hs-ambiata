module Disorder.Core.QuickCheck (
    module X
  ) where

-- the "dropbear" .&. is bad, and you should feel bad
import           Test.QuickCheck as X hiding ((.&.))
