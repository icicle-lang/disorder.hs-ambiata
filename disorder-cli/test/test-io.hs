import           Control.Monad

import qualified Test.IO.Disorder.Cli.Shell

import           System.Exit
import           System.IO

main :: IO ()
main = hSetBuffering stdout LineBuffering >>
  sequence [
      Test.IO.Disorder.Cli.Shell.tests
  ] >>= \rs -> unless (and rs) exitFailure

