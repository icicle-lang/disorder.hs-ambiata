import           Control.Monad

import qualified Orphanarium.Core.PropertyTest

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> sequence
    [ Orphanarium.Core.PropertyTest.tests
    ] >>= \rs -> unless (and rs) exitFailure
