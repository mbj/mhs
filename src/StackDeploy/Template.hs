module StackDeploy.Template
  ( Name
  , Provider
  , Template(..)
  , encode
  , get
  , mk
  , mkName
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Lazy (ByteString)
import Data.Ord (compare)
import StackDeploy.Prelude

import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified StackDeploy.Provider     as Provider
import qualified Stratosphere

type Name = Provider.Name Template

data Template = Template
  { name         :: Name
  , stratosphere :: Stratosphere.Template
  }

instance Provider.HasName Template where
  name = name

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
get = Provider.get "template"

mk :: Name -> Stratosphere.Template -> Template
mk = Template

mkName :: Text -> Name
mkName = Provider.mkName
