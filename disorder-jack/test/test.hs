import           Disorder.Core.Main

import qualified Test.Disorder.Jack.Combinators
import qualified Test.Disorder.Jack.Minimal
import qualified Test.Disorder.Jack.Property.Diff
import qualified Test.Disorder.Jack.Shrink
import qualified Test.Disorder.Jack.Tree

main :: IO ()
main =
  disorderMain [
      Test.Disorder.Jack.Combinators.tests
    , Test.Disorder.Jack.Minimal.tests
    , Test.Disorder.Jack.Property.Diff.tests
    , Test.Disorder.Jack.Shrink.tests
    , Test.Disorder.Jack.Tree.tests
    ]
