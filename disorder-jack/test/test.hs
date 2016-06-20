import           Disorder.Core.Main

import qualified Test.Disorder.Jack.Shrink
import qualified Test.Disorder.Jack.Tree

main :: IO ()
main =
  disorderMain [
      Test.Disorder.Jack.Shrink.tests
    , Test.Disorder.Jack.Tree.tests
    ]
