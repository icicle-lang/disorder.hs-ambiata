import           Control.Monad

import           System.Exit
import           System.IO

--import qualified Test.Disorder.Species.Gen
--import qualified Test.Disorder.Spe.Gen
import qualified Test.Disorder.Sp.Gen

main :: IO ()
main = disorderMain [
--    Test.Disorder.Species.Gen.tests
--   Test.Disorder.Spe.Gen.tests
   Test.Disorder.Sp.Gen.tests
  ]

disorderMain :: [IO Bool] -> IO ()
disorderMain tests =
  sanity >> sequence tests >>= \rs -> unless (and rs) exitFailure

sanity :: IO ()
sanity =
  hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering
