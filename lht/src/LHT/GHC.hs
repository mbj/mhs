module LHT.GHC (majorMinor) where

import LHT.Prelude

import qualified Data.List as List
import qualified Data.Text as Text
import qualified GHC.Version

majorMinor :: Text
majorMinor
  = Text.intercalate "."
  . List.take 2
  . Text.splitOn "."
  $ convert GHC.Version.cProjectVersion
