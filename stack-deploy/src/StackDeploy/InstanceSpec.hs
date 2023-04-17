module StackDeploy.InstanceSpec
  ( InstanceSpec(..)
  , Name
  , Provider
  , RoleARN(..)
  , addTags
  , get
  , mk
  , mkName
  , templateProvider
  )
where

import Data.MonoTraversable (omap)
import Prelude (error)
import StackDeploy.Parameters (Parameters)
import StackDeploy.Prelude
import StackDeploy.Template (Template)

import qualified Amazonka.CloudFormation.Types as CF
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.KeyMap             as KeyMap
import qualified StackDeploy.Parameters        as Parameters
import qualified StackDeploy.Provider          as Provider
import qualified StackDeploy.Template          as Template
import qualified Stratosphere

newtype RoleARN = RoleARN Text
  deriving (Conversion Text) via Text
  deriving stock Eq

type Name env = Provider.Name (InstanceSpec env)

type Provider env = Provider.Provider (InstanceSpec env)

data InstanceSpec env = InstanceSpec
  { capabilities  :: [CF.Capability]
  , envParameters :: MIO env Parameters
  , envRoleARN    :: Maybe (MIO env RoleARN)
  , name          :: Name env
  , onSuccess     :: MIO env ()
  , parameters    :: Parameters
  , roleARN       :: Maybe RoleARN
  , template      :: Template
  }

instance Provider.HasName (InstanceSpec env) where
  name = (.name)

get
  :: Provider env
  -> Name env
  -> Parameters
  -> MIO env (InstanceSpec env)
get provider targetName userParameters = do
  instanceSpec <- Provider.get "instance-spec" provider targetName
  env          <- instanceSpec.envParameters
  roleARN      <- tryEnvRole instanceSpec

  pure $ instanceSpec
    { parameters
        =  expandedParameters instanceSpec
        `union` env
        `union` userParameters
    , roleARN  = roleARN
    }

  where
    expandedParameters :: InstanceSpec env -> Parameters
    expandedParameters InstanceSpec{..} =
      Parameters.expandTemplate parameters template

    tryEnvRole :: InstanceSpec env -> MIO env (Maybe RoleARN)
    tryEnvRole InstanceSpec{..} = maybe (pure roleARN) (pure <$>) envRoleARN

    union = Parameters.union

mk :: Name env -> Template -> InstanceSpec env
mk name template = InstanceSpec
  { capabilities  = empty
  , envParameters = pure Parameters.empty
  , envRoleARN    = empty
  , onSuccess     = pure ()
  , parameters    = Parameters.empty
  , roleARN       = empty
  , ..
  }

mkName :: Text -> Name env
mkName = Provider.mkName

templateProvider :: Provider env -> Template.Provider
templateProvider provider = fromList $ (.template) <$> toList provider

addTags :: [Stratosphere.Tag] -> InstanceSpec env -> InstanceSpec env
addTags tags InstanceSpec{..}
  = InstanceSpec
  { template = addTemplateTags tags template
  , ..
  }

addTemplateTags :: [Stratosphere.Tag] -> Template.Template -> Template.Template
addTemplateTags tags Template.Template{..}
  = Template.Template
  { stratosphere = addStratosphereTags tags stratosphere
  , ..
  }

addStratosphereTags :: [Stratosphere.Tag] -> Stratosphere.Template -> Stratosphere.Template
addStratosphereTags tags Stratosphere.Template{..}
  = Stratosphere.Template
  { resources = omap (addResourceTags tags) resources
  , ..
  }

addResourceTags :: [Stratosphere.Tag] -> Stratosphere.Resource -> Stratosphere.Resource
addResourceTags tags Stratosphere.Resource{..}
  = Stratosphere.Resource
  { properties = addResourcePropertiesTags tags properties
  , ..
  }

addResourcePropertiesTags
  :: [Stratosphere.Tag]
  -> Stratosphere.ResourceProperties
  -> Stratosphere.ResourceProperties
addResourcePropertiesTags tags Stratosphere.ResourceProperties{..}
  = Stratosphere.ResourceProperties
  { properties = newProperties
  , ..
  }
  where
    newProperties =
      if supportsTags
        then KeyMap.unionWith merge [("Tags", JSON.toJSON tags)] properties
        else properties

    merge leftValue rightValue = case (leftValue, rightValue) of
      (JSON.Array leftItems, JSON.Array rightItems) -> JSON.Array $ leftItems <> rightItems
      other                                         -> error $ "non tag array merge:" <> show other
