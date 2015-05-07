import           Control.Monad

import qualified Test.Disorder.Corpus

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.Disorder.Corpus.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
