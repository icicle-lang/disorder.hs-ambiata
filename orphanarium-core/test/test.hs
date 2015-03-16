import           Orphanarium.Core.Main
import qualified Orphanarium.Core.PropertyTest

main :: IO ()
main = orphanariumMain [Orphanarium.Core.PropertyTest.tests]
