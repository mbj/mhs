module Devtools.Dependencies (testTree) where

import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Text (Text)
import System.IO (IO)

import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text.Encoding    as Text
import qualified System.Process.Typed  as Process
import qualified Test.Tasty            as Tasty
import qualified Test.Tasty.MGolden    as Tasty

testTree :: Tasty.TestTree
testTree =
  Tasty.goldenTest
    "dependencies"
    "test/stack-dependencies.txt"
    readDependenciesText
  where
    readDependenciesText :: IO Text
    readDependenciesText
      = Text.decodeUtf8 . LBS.toStrict <$> readDependencies

    readDependencies :: IO LBS.ByteString
    readDependencies
      = Process.readProcessStdout_
      $ Process.proc "stack" ["ls", "dependencies", "--test"]
