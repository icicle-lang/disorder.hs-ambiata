import           Control.Monad

import qualified Test.Disorder.Aeson
import qualified Test.Disorder.Aeson.Time

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.Disorder.Aeson.tests
    , Test.Disorder.Aeson.Time.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
