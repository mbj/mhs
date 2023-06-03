module StackDeploy.Template
  ( Name
  , Provider
  , Template(..)
  , encode
  , get
  , mk
  , testTree
  )
where

import Data.Ord (compare)
import StackDeploy.Prelude
import System.FilePath ((<.>), (</>))

import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Text.Encoding       as Text
import qualified StackDeploy.Provider     as Provider
import qualified Stratosphere
import qualified Test.Tasty               as Tasty
import qualified Test.Tasty.MGolden       as Tasty

type Name = BoundText "StackDeploy.Template.Name"

data Template = Template
  { name         :: Name
  , stratosphere :: Stratosphere.Template
  }

instance Provider.HasItemName Template where
  type ItemName Template = Name
  name = (.name)

type Provider = Provider.Provider Template

-- | Pretty print a template using aeson-pretty.
encode :: Template -> LBS.ByteString
encode = Pretty.encodePretty' config . (.stratosphere)
  where
    config = Pretty.defConfig
      { Pretty.confIndent  = Pretty.Spaces 2
      , Pretty.confCompare = compare
      }

get :: MonadThrow m => Provider -> Name -> m Template
get = Provider.get "template"

mk :: Name -> Stratosphere.Template -> Template
mk = Template

testTree :: Provider -> Tasty.TestTree
testTree provider
  = Tasty.testGroup "template" (templateTest <$> toList provider)

templateTest :: Template -> Tasty.TestTree
templateTest template@Template{..} =
  Tasty.goldenTest
    (convertText name)
    expectedPath
    (pure . Text.decodeUtf8 . LBS.toStrict $ encode template)
  where
    expectedPath = "test" </> "template" </> convertText name <.> ".json"
