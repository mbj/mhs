module Devtools.Dependencies
  ( getFilename
  , testTree
  )
where

import Data.Tuple (fst)
import Devtools.Config
import Devtools.Prelude
import System.FilePath (FilePath, (</>))

import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text.Encoding    as Text
import qualified System.Environment    as Environment
import qualified System.FilePath       as FilePath
import qualified System.Process.Typed  as Process
import qualified Test.Tasty            as Tasty
import qualified Test.Tasty.MGolden    as Tasty

testTree :: FilePath -> [Target] -> Tasty.TestTree
testTree filename targets =
  Tasty.goldenTest "dependencies" filename readDependenciesText
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
      ] <> (convert <$> targets)

getFilename :: IO FilePath
getFilename = do
  prefix <- getPrefix
  pure $ "test" </> prefix <> "-dependencies.txt"
  where
    getPrefix
      =   maybe "stack" (FilePath.dropExtension . FilePath.takeFileName)
      <$> Environment.lookupEnv "STACK_YAML"
