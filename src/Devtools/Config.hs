module Devtools.Config where

import Devtools.Prelude

import qualified Data.Text as Text

newtype Target = Target Text

targetString :: Target -> String
targetString (Target text) = Text.unpack text

data Config = Config
  { hlintArguments :: [String]
  , targets        :: [Target]
  }
