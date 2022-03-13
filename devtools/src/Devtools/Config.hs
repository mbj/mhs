{-# LANGUAGE DerivingVia #-}

module Devtools.Config where

import Devtools.Prelude

newtype Target = Target Text
  deriving (Conversion String) via Text

data Config = Config
  { hlintArguments :: [String]
  , targets        :: [Target]
  }
