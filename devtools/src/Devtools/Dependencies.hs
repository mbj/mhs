{-# LANGUAGE DeriveLift #-}

module Devtools.Dependencies
  ( Dependencies
  , Target(..)
  , dependencyTestTree
  , getDependencies
  )
where

import Data.Tuple (fst)
import Devtools.Prelude
import Prelude (error)
import System.FilePath (FilePath, (</>))

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.List                as List
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Language.Haskell.TH.Lift as TH
import qualified System.Process.Typed     as Process
import qualified Test.Tasty               as Tasty
import qualified Test.Tasty.MGolden       as Tasty

newtype Target = Target Text

targetString :: Target -> String
targetString (Target text) = Text.unpack text

data Dependencies = Dependencies
  { path     :: FilePath
  , expected :: Text
  }
  deriving stock TH.Lift

dependencyTestTree :: Dependencies -> Tasty.TestTree
dependencyTestTree Dependencies{..} = Tasty.goldenTest "dependencies" path (pure expected)

getDependencies :: [Target] -> IO Dependencies
getDependencies targets = do
  expected <- getExpected targets
  path     <- getPath
  pure Dependencies{..}

getPath :: IO FilePath
getPath = do
  prefix <- getPrefix
  pure $ "test" </> "stack-" <> prefix <> "-dependencies.txt"
  where
    getPrefix :: IO String
    getPrefix = Text.unpack . majorMinor . List.last . Text.words . decode . fst
      <$> Process.readProcess_ (Process.proc "stack" ["ghc", "--", "--version"])

    majorMinor :: Text -> Text
    majorMinor input = case Text.split (== '.') input of
      [major,minor,_] -> major <> "." <> minor
      _               -> error "Unexpected GHC version format"

getExpected :: [Target] -> IO Text
getExpected targets = decode . fst <$> Process.readProcess_ (Process.proc "stack" arguments)
  where
    arguments :: [String]
    arguments =
      [ "ls"
      , "dependencies"
      , "--test"
      ] <> (targetString <$> targets)

decode :: LBS.ByteString -> Text
decode = Text.decodeUtf8 . LBS.toStrict
