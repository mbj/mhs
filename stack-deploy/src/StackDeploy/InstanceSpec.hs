module StackDeploy.InstanceSpec
  ( InstanceName
  , InstanceSpec(..)
  , InstanceSpecMap
  , RoleARN
  , addTags
  , fetchInstanceSpec
  , instanceSpecMapFromList
  , instanceSpecMapTestTree
  , mkInstanceSpec
  , setInstanceSpecParameter
  , templateMapFromInstanceSpecMap
  )
where

import Data.MonoTraversable (omap)
import Prelude (error)
import StackDeploy.Prelude

import qualified Amazonka.CloudFormation.Types as CF
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.KeyMap             as KeyMap
import qualified Data.Map.Strict               as Map
import qualified StackDeploy.NamedTemplate     as StackDeploy
import qualified StackDeploy.Parameters        as StackDeploy
import qualified Stratosphere                  as CFT
import qualified Test.Tasty                    as Tasty

-- $setup
-- >>> import StackDeploy.Prelude
-- >>> import qualified StackDeploy.InstanceSpec  as StackDeploy
-- >>> import qualified StackDeploy.NamedTemplate as StackDeploy
-- >>> import qualified Stratosphere              as CFT

type RoleARN             = BoundText "StackDeploy.InstanceSpec.RoleARN"
type InstanceName        = BoundText "StackDeploy.InstanceSpec.Name"
type InstanceSpecMap env = Map InstanceName (InstanceSpec env)

data InstanceSpec env = InstanceSpec
  { capabilities  :: [CF.Capability]
  , name          :: InstanceName
  , namedTemplate :: StackDeploy.NamedTemplate
  , onLoad        :: InstanceSpec env -> MIO env (InstanceSpec env)
  , onSuccess     :: MIO env ()
  , parameterMap  :: StackDeploy.ParameterMap
  , roleArn       :: Maybe RoleARN
  }

fetchInstanceSpec :: MonadIO m => InstanceName -> InstanceSpecMap env -> m (InstanceSpec env)
fetchInstanceSpec instanceName = maybe absent pure . Map.lookup instanceName
  where
    absent = throwString $ "Instance spec not known: " <> convertVia @Text instanceName

-- | Set instance spec parameter
setInstanceSpecParameter :: StackDeploy.Parameter -> InstanceSpec env -> InstanceSpec env
setInstanceSpecParameter parameter instanceSpec@InstanceSpec{..}
  = instanceSpec
  { parameterMap = Map.insert parameter.name parameter.value parameterMap
  }

-- | Construct instance spec map from list
-- >>> let namedTemplate = StackDeploy.mkNamedTemplate (fromType @"test") (CFT.mkTemplate [])
-- >>> let instanceSpec = StackDeploy.mkInstanceSpec (fromType @"test") namedTemplate
-- >>> let instanceSpecMap = StackDeploy.instanceSpecMapFromList [instanceSpec]
instanceSpecMapFromList :: [InstanceSpec env] -> InstanceSpecMap env
instanceSpecMapFromList = Map.fromList . fmap (\instanceSpec -> (instanceSpec.name, instanceSpec))

-- | Construct minimal instance spec
-- >>> let namedTemplate = StackDeploy.mkNamedTemplate (fromType @"test") (CFT.mkTemplate [])
-- >>> let instanceSpec = StackDeploy.mkInstanceSpec (fromType @"test") namedTemplate
-- >>> instanceSpec.capabilities
-- []
-- >>> instanceSpec.name
-- BoundText "test"
-- >>> instanceSpec.parameterMap
-- fromList []
-- >>> instanceSpec.roleArn
-- Nothing
mkInstanceSpec :: InstanceName -> StackDeploy.NamedTemplate -> InstanceSpec env
mkInstanceSpec name namedTemplate = InstanceSpec
  { capabilities  = empty
  , onLoad        = pure
  , onSuccess     = pure ()
  , parameterMap  = []
  , roleArn       = empty
  , ..
  }

-- | Construct template map from instance spec map
-- >>> let namedTemplate = StackDeploy.mkNamedTemplate (fromType @"test") (CFT.mkTemplate [])
-- >>> let instanceSpec  = StackDeploy.mkInstanceSpec (fromType @"test") namedTemplate
-- >>> StackDeploy.templateMapFromInstanceSpecMap [((fromType @"test"), instanceSpec)]
-- fromList [(BoundText "test",Template {conditions = Nothing, description = Nothing, formatVersion = Nothing, mappings = Nothing, metadata = Nothing, outputs = Nothing, parameters = Nothing, resources = Resources {resourceList = []}})]
templateMapFromInstanceSpecMap :: InstanceSpecMap env -> StackDeploy.NamedTemplateMap
templateMapFromInstanceSpecMap map
  = Map.fromList
  $ (\InstanceSpec{..} -> (namedTemplate.name, namedTemplate.template)) <$> Map.elems map

-- | Add static tags to instance spec
addTags :: [CFT.Tag] -> InstanceSpec env -> InstanceSpec env
addTags tags InstanceSpec{..}
  = InstanceSpec
  { namedTemplate = addNamedTemplateTags tags namedTemplate
  , ..
  }

-- | Add static tags to named template tags
addNamedTemplateTags :: [CFT.Tag] -> StackDeploy.NamedTemplate -> StackDeploy.NamedTemplate
addNamedTemplateTags tags StackDeploy.NamedTemplate{..}
  = StackDeploy.NamedTemplate
  { template = addStratosphereTags tags template
  , ..
  }

-- | Add static tags to stratosphere template
addStratosphereTags :: [CFT.Tag] -> CFT.Template -> CFT.Template
addStratosphereTags tags CFT.Template{..}
  = CFT.Template
  { resources = omap (addResourceTags tags) resources
  , ..
  }

-- | Add static tags to stratosphere resource
addResourceTags :: [CFT.Tag] -> CFT.Resource -> CFT.Resource
addResourceTags tags CFT.Resource{..}
  = CFT.Resource
  { properties = addResourcePropertiesTags tags properties
  , ..
  }

-- | Add static tags to stratosphere resource properties
--
-- This function will honor the special logic for aws auto scaling group and set tags to be propagated on launch.
addResourcePropertiesTags
  :: [CFT.Tag]
  -> CFT.ResourceProperties
  -> CFT.ResourceProperties
addResourcePropertiesTags tags CFT.ResourceProperties{..}
  = CFT.ResourceProperties
  { properties = newProperties
  , ..
  }
  where
    addPropagateAtLaunch value =
      if awsType == "AWS::AutoScaling::AutoScalingGroup"
        then mergeArrayObject value (JSON.Object [("PropagateAtLaunch", JSON.toJSON True)])
        else value

    newProperties =
      if supportsTags
        then KeyMap.unionWith mergeArray [("Tags", addPropagateAtLaunch $ JSON.toJSON tags)] properties
        else properties

    mergeArrayObject leftValue rightValue = case leftValue of
      JSON.Array leftItems -> JSON.Array $ mergeObject rightValue <$> leftItems
      other                -> error $ "invalid array merge:" <> show (other, rightValue)

    mergeObject leftValue rightValue = case (leftValue, rightValue) of
      (JSON.Object leftMap, JSON.Object rightMap) -> JSON.Object $ leftMap <> rightMap
      other                                       -> error $ "invalid object merge:" <> show other

    mergeArray leftValue rightValue = case (leftValue, rightValue) of
      (JSON.Array leftItems, JSON.Array rightItems) -> JSON.Array $ leftItems <> rightItems
      other                                         -> error $ "invalid array merge:" <> show other

instanceSpecMapTestTree :: InstanceSpecMap env -> Tasty.TestTree
instanceSpecMapTestTree = StackDeploy.namedTemplateMapTestTree . templateMapFromInstanceSpecMap
