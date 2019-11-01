import MPrelude
import System.IO (IO)
import Test.Tasty.HUnit

import qualified DBT.Podman as Podman
import qualified Devtools
import qualified Test.Tasty as Tasty

main :: IO ()
main
  = Tasty.defaultMain
  $ Tasty.testGroup "dbt" [Devtools.testTree, image]

image :: Tasty.TestTree
image
  = testCase "image" . void $ Podman.getImage
