module StackDeploy.InstanceSpec
  ( InstanceSpec(..)
  , Name
  , Provider
  , RoleARN(..)
  , get
  , mk
  , mkName
  , templateProvider
  )
where

import StackDeploy.Parameters (Parameters)
import StackDeploy.Prelude
import StackDeploy.Template (Template)

import qualified Network.AWS.CloudFormation.Types as CF
import qualified StackDeploy.Parameters           as Parameters
import qualified StackDeploy.Provider             as Provider
import qualified StackDeploy.Template             as Template

newtype RoleARN = RoleARN Text
  deriving (Conversion Text) via Text
  deriving stock Eq

type Name env = Provider.Name (InstanceSpec env)

type Provider env = Provider.Provider (InstanceSpec env)

data InstanceSpec env = InstanceSpec
  { capabilities  :: [CF.Capability]
  , envParameters :: RIO env Parameters
  , envRoleARN    :: Maybe (RIO env RoleARN)
  , name          :: Name env
  , onSuccess     :: RIO env ()
  , parameters    :: Parameters
  , roleARN       :: Maybe RoleARN
  , template      :: Template
  }

instance Provider.HasName (InstanceSpec env) where
  name = name

get
  :: Provider env
  -> Name env
  -> Parameters
  -> RIO env (InstanceSpec env)
get provider targetName userParameters = do
  instanceSpec <- Provider.get "instance-spec" provider targetName
  env          <- envParameters instanceSpec
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

    tryEnvRole :: InstanceSpec env -> RIO env (Maybe RoleARN)
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
templateProvider provider = fromList $ template <$> toList provider
