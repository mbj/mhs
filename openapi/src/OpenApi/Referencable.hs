module OpenApi.Referencable where

import OpenApi.Prelude

class Referencable a where
  targetName    :: String
  referencePath :: [Text]
