import           Control.Monad

import qualified Orphanarium.LensTest

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> sequence [
      Orphanarium.LensTest.tests
    ] >>= \rs -> unless (and rs) exitFailure
