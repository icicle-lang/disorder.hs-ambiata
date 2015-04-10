import           Orphanarium.Core.Main
import qualified Orphanarium.Core.PropertyTest
import qualified Orphanarium.Test.Core.UniquePair

main :: IO ()
main = orphanariumMain [
    Orphanarium.Core.PropertyTest.tests
  , Orphanarium.Test.Core.UniquePair.tests
  ]
