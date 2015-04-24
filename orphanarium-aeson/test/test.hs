import           Control.Monad

import qualified Test.Orphanarium.Aeson

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.Orphanarium.Aeson.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
