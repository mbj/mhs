module StackDeploy.Template (encodeTemplate) where

import Data.ByteString.Lazy (ByteString)
import Data.Ord (compare)

import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Stratosphere

-- | Pretty print a template using aeson-pretty.
encodeTemplate :: Stratosphere.Template -> ByteString
encodeTemplate = Pretty.encodePretty' config
  where
    config = Pretty.defConfig
      { Pretty.confIndent  = Pretty.Spaces 2
      , Pretty.confCompare = compare
      }
