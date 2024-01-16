module StackDeploy.NamedTemplate
  ( NamedTemplate(..)
  , NamedTemplateMap
  , TemplateName
  , mkNamedTemplate
  , namedTemplateMapFromList
  , namedTemplateMapTestTree
  , namedTemplateParameterNames
  , stratosphereTemplateEncodePretty
  )
where

import Data.Ord (compare)
import StackDeploy.Prelude
import System.FilePath ((<.>), (</>))

import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Data.Text.Encoding       as Text
import qualified StackDeploy.Parameters   as StackDeploy
import qualified Stratosphere             as CFT
import qualified Test.Tasty               as Tasty
import qualified Test.Tasty.MGolden       as Tasty

-- $setup
-- >>> import StackDeploy.Prelude
-- >>> import qualified Stratosphere              as CFT
-- >>> import qualified StackDeploy.NamedTemplate as StackDeploy

type TemplateName = BoundText "StackDeploy.Template.Name"

data NamedTemplate = NamedTemplate
  { name     :: TemplateName
  , template :: CFT.Template
  }
  deriving stock (Eq, Show)

type NamedTemplateMap = Map TemplateName CFT.Template

-- | Pretty print a template using aeson-pretty.
-- >>> StackDeploy.stratosphereTemplateEncodePretty $ CFT.mkTemplate []
-- "{\n  \"Resources\": {}\n}"
stratosphereTemplateEncodePretty :: CFT.Template -> LBS.ByteString
stratosphereTemplateEncodePretty = JSON.encodePretty' config
  where
    config
      = JSON.defConfig
      { JSON.confIndent  = JSON.Spaces 2
      , JSON.confCompare = compare
      }

-- | Construct named template
-- >>> StackDeploy.mkNamedTemplate (fromType @"test") (CFT.mkTemplate [])
-- NamedTemplate {name = BoundText "test", template = Template {conditions = Nothing, description = Nothing, formatVersion = Nothing, mappings = Nothing, metadata = Nothing, outputs = Nothing, parameters = Nothing, resources = Resources {resourceList = []}}}
mkNamedTemplate :: TemplateName -> CFT.Template -> NamedTemplate
mkNamedTemplate = NamedTemplate

-- | Construct template map from named templates
-- >>> let test = StackDeploy.mkNamedTemplate (fromType @"test") (CFT.mkTemplate [])
-- >>> StackDeploy.namedTemplateMapFromList [test]
-- fromList [(BoundText "test",Template {conditions = Nothing, description = Nothing, formatVersion = Nothing, mappings = Nothing, metadata = Nothing, outputs = Nothing, parameters = Nothing, resources = Resources {resourceList = []}})]
namedTemplateMapFromList :: [NamedTemplate] -> NamedTemplateMap
namedTemplateMapFromList = Map.fromList . fmap (\NamedTemplate{..} -> (name, template))

-- | Parameter names from named template
-- >>> let cftParameterA = CFT.mkParameter "A" "String" & CFT.set @"Default" "A-Default"
-- >>> let cftParameterB = CFT.mkParameter "B" "String"
-- >>> let cftTemplate0  = CFT.mkTemplate []
-- >>> let cftTemplate1  = CFT.mkTemplate [] & CFT.set @"Parameters" [cftParameterA, cftParameterB]
-- >>> StackDeploy.namedTemplateParameterNames $ StackDeploy.mkNamedTemplate (fromType @"test") cftTemplate0
-- fromList []
-- >>> StackDeploy.namedTemplateParameterNames $ StackDeploy.mkNamedTemplate (fromType @"test") cftTemplate1
-- fromList [BoundText "A",BoundText "B"]
namedTemplateParameterNames :: NamedTemplate -> Set StackDeploy.ParameterName
namedTemplateParameterNames namedTemplate
  = Set.fromList
  $ (\CFT.Parameter{..} -> convertImpure name) <$> maybe [] (.parameterList) namedTemplate.template.parameters

namedTemplateMapTestTree :: NamedTemplateMap -> Tasty.TestTree
namedTemplateMapTestTree map = Tasty.testGroup "template" (templateTest <$> Map.toList map)
  where
    templateTest :: (TemplateName, CFT.Template) -> Tasty.TestTree
    templateTest (name, template) =
      Tasty.goldenTest
        (convertText name)
        expectedPath
        (pure . Text.decodeUtf8 . LBS.toStrict $ stratosphereTemplateEncodePretty template)
      where
        expectedPath = "test" </> "template" </> convertText name <.> ".json"
