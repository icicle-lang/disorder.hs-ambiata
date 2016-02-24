import           Disorder.Core.Main
import qualified Test.Disorder.Core.Gen
import qualified Test.Disorder.Core.Property
import qualified Test.Disorder.Core.Tripping
import qualified Test.Disorder.Core.UniquePair
import qualified Test.Disorder.Core.OrdPair
import qualified Test.Disorder.Core.IO

main :: IO ()
main = disorderMain [
    Test.Disorder.Core.Gen.tests
  , Test.Disorder.Core.Property.tests
  , Test.Disorder.Core.Tripping.tests
  , Test.Disorder.Core.UniquePair.tests
  , Test.Disorder.Core.OrdPair.tests
  , Test.Disorder.Core.IO.tests
  ]
