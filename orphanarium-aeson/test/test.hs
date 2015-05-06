import           Control.Monad

import qualified Test.Orphanarium.Aeson
import qualified Test.Orphanarium.Aeson.Time

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.Orphanarium.Aeson.tests
    , Test.Orphanarium.Aeson.Time.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
