import           Disorder.Core.Main
import qualified Test.Disorder.Core.Property
import qualified Test.Disorder.Core.UniquePair
import qualified Test.Disorder.Core.NaturalInt
import qualified Test.Disorder.Core.PositiveInt

main :: IO ()
main = disorderMain [
    Test.Disorder.Core.Property.tests
  , Test.Disorder.Core.UniquePair.tests
  , Test.Disorder.Core.NaturalInt.tests
  , Test.Disorder.Core.PositiveInt.tests
  ]
