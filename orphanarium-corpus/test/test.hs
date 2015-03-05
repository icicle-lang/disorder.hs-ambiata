import           Control.Monad

import qualified Orphanarium.CorpusTest

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Orphanarium.CorpusTest.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
