import           Control.Monad

import qualified Test.Orphanarium.Lens

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> sequence [
      Test.Orphanarium.Lens.tests
    ] >>= \rs -> unless (and rs) exitFailure
