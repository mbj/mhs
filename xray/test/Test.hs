module Main (main) where

import MPrelude
import Test.Tasty

import qualified Devtools
import qualified Test.TraceHeader as TraceHeader

main :: IO ()
main =
  defaultMain $ testGroup "xray"
    [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "xray"])
    , TraceHeader.testTree
    ]
