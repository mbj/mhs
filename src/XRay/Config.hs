module XRay.Config (Config(..), defaultConfig) where

import XRay.Prelude
import XRay.Segment

-- | XRay configuration
data Config = Config
  { annotations       :: Maybe Annotations
  , aws               :: Maybe Aws
  , origin            :: Maybe Origin
  , service           :: Maybe Service
  }

defaultConfig :: Config
defaultConfig = do
  Config
    { annotations = empty
    , aws         = empty
    , origin      = empty
    , service     = empty
    , ..
    }
