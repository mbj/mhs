module Devtools.Dependencies (testTree) where

import Data.Tuple (fst)
import Devtools.Config
import Devtools.Prelude

import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text.Encoding    as Text
import qualified System.Process.Typed  as Process
import qualified Test.Tasty            as Tasty
import qualified Test.Tasty.MGolden    as Tasty

testTree :: [Target] -> Tasty.TestTree
testTree targets =
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
      = fst <$> Process.readProcess_ (Process.proc "stack" arguments)

    arguments :: [String]
    arguments =
      [ "ls"
      , "dependencies"
      , "--test"
      ] <> (targetString <$> targets)
