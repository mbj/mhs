module Main (main) where
import MPrelude

import qualified Devtools
import qualified Test.Network.HTTP.Mclient
import qualified Test.Tasty                as Tasty

main :: IO ()
main =
  liftIO . Tasty.defaultMain .
    Tasty.testGroup "HTTP" $
      [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "http-mclient"])
      , Test.Network.HTTP.Mclient.testTree
      ]
