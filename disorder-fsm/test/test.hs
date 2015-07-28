import           Control.Monad

import qualified Test.Disorder.FSM

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> sequence [
      Test.Disorder.FSM.tests
    ] >>= \rs -> unless (and rs) exitFailure
