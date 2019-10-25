module StackDeploy.InstanceSpec
  ( InstanceSpec(..)
  , Name(..)
  , Provider
  , RoleARN(..)
  , get
  , mk
  , templateProvider
  )
where

import StackDeploy.AWS
import StackDeploy.Parameters (Parameters)
import StackDeploy.Prelude
import StackDeploy.Template (Template)

import qualified Network.AWS.CloudFormation.Types as CF
import qualified StackDeploy.Parameters           as Parameters
import qualified StackDeploy.Provider             as Provider
import qualified StackDeploy.Template             as Template

newtype Name = Name Text
  deriving newtype ToText
  deriving stock   Eq

newtype RoleARN = RoleARN Text
  deriving newtype ToText
  deriving stock   Eq

type Provider = Provider.Provider InstanceSpec

data InstanceSpec = InstanceSpec
  { capabilities  :: [CF.Capability]
  , envParameters :: forall m r . (AWSConstraint r m, MonadAWS m) => m Parameters
  , envRoleARN    :: forall m r . (AWSConstraint r m, MonadAWS m) => Maybe (m RoleARN)
  , name          :: Name
  , onSuccess     :: forall m r . (AWSConstraint r m, MonadAWS m) => m ()
  , parameters    :: Parameters
  , roleARN       :: Maybe RoleARN
  , template      :: Template
  }

get
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => Provider
  -> Name
  -> Parameters
  -> m InstanceSpec
get provider targetName userParameters = do
  instanceSpec <- Provider.get "instance-spec" name provider targetName
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
    expandedParameters :: InstanceSpec -> Parameters
    expandedParameters InstanceSpec{..} =
      Parameters.expandTemplate parameters template

    tryEnvRole :: InstanceSpec -> m (Maybe RoleARN)
    tryEnvRole InstanceSpec{..} = maybe (pure roleARN) (pure <$>) envRoleARN

    union = Parameters.union

mk :: Name -> Template -> InstanceSpec
mk name template = InstanceSpec
  { capabilities  = empty
  , envParameters = pure Parameters.empty
  , envRoleARN    = empty
  , onSuccess     = pure ()
  , parameters    = Parameters.empty
  , roleARN       = empty
  , ..
  }

templateProvider :: Provider -> Template.Provider
templateProvider provider = template <$> provider
