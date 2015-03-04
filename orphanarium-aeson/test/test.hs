import           Control.Monad

import qualified Orphanarium.AesonTest

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Orphanarium.AesonTest.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
