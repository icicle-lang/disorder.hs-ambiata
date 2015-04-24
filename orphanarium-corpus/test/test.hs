import           Control.Monad

import qualified Test.Orphanarium.Corpus

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.Orphanarium.Corpus.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
