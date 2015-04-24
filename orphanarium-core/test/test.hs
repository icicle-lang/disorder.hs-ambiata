import           Orphanarium.Core.Main
import qualified Test.Orphanarium.Core.Property
import qualified Test.Orphanarium.Core.UniquePair

main :: IO ()
main = orphanariumMain [
    Test.Orphanarium.Core.Property.tests
  , Test.Orphanarium.Core.UniquePair.tests
  ]
