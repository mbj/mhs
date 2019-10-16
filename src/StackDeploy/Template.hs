module StackDeploy.Template
  ( Name(..)
  , Provider
  , Template(..)
  , encode
  , get
  , mk
  ) where

import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Lazy (ByteString)
import Data.Ord (compare)
import StackDeploy.Prelude

import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified StackDeploy.Provider     as Provider
import qualified Stratosphere

newtype Name = Name Text
  deriving newtype ToText
  deriving stock   Eq

data Template = Template
  { name         :: Name
  , stratosphere :: Stratosphere.Template
  }

type Provider = Provider.Provider Template

-- | Pretty print a template using aeson-pretty.
encode :: Template -> ByteString
encode = Pretty.encodePretty' config . stratosphere
  where
    config = Pretty.defConfig
      { Pretty.confIndent  = Pretty.Spaces 2
      , Pretty.confCompare = compare
      }

get :: MonadThrow m => Provider -> Name -> m Template
get = Provider.get "template" name

mk :: Name -> Stratosphere.Template -> Template
mk = Template
