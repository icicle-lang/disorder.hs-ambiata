import           Control.Monad

import qualified Test.Disorder.Lens

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> sequence [
      Test.Disorder.Lens.tests
    ] >>= \rs -> unless (and rs) exitFailure
