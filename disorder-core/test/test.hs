import           Disorder.Core.Main
import qualified Test.Disorder.Core.Property
import qualified Test.Disorder.Core.UniquePair

main :: IO ()
main = disorderMain [
    Test.Disorder.Core.Property.tests
  , Test.Disorder.Core.UniquePair.tests
  ]
