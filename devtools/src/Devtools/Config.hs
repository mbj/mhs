{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module Devtools.Config where

import Devtools.Prelude

newtype Target = Target Text
  deriving (Conversion String) via Text

type StackFlag = (BoundText "FlagName", BoundText "FlagValue")

data Config = Config
  { hlintArguments :: [String]
  , stackFlags     :: [StackFlag]
  , targets        :: [Target]
  }
