module StackDeploy.EnvSpec
  ( EnvSpec(..)
  , EnvSpecName
  , EnvSpecValue(..)
  , envSpecEcsTaskDefinitionEnvironment
  , envSpecLambdaEnvironment
  , envSpecPosixEnvironment
  , readEnvSpecFromEnvironment
  , readEnvSpecFromStack
  )
where

import StackDeploy.Prelude

import qualified Amazonka.CloudFormation.Types   as CF
import qualified Data.Foldable                   as Foldable
import qualified Data.List                       as List
import qualified StackDeploy.Stack               as StackDeploy
import qualified StackDeploy.Stratosphere        as CFT
import qualified Stratosphere                    as CFT
import qualified Stratosphere.ECS.TaskDefinition as ECS.TaskDefinition
import qualified Stratosphere.Lambda.Function    as Lambda.Function
import qualified UnliftIO.Environment            as Environment

-- $setup
-- >>> import StackDeploy.Prelude
-- >>> import qualified Data.Time.Clock               as Time
-- >>> import qualified Data.Time.Clock.POSIX         as Time
-- >>> import qualified Amazonka.CloudFormation.Types as CF
-- >>> import qualified StackDeploy.EnvSpec           as StackDeploy

-- | Specification of an environment variable that can be evaluated inside and outside of cloudformation defined resources.
--
-- Primary use case is to be able to run local operations CLI tooling in the same
-- environment stack defined resources are running into.
-- As long the stack defined resources are being defined via this data type its
-- guaranteed an execution on a developers/operators machine can reproduc the same
-- environment.
--
-- General useage is to export env spec values from components for subsystems,
-- like modules that define lambas and ecs containers. Than for local ops tooling that
-- needs the same environment run load these env spec values directly via functions like
-- `readenvSpecFromStack` outside of AWS execution resources.
data EnvSpec = EnvSpec
  { name  :: EnvSpecName
  , value :: EnvSpecValue
  }

type EnvSpecName = BoundText "StackDeploy.EnvSpec.Name"

data EnvSpecValue
  = EnvSpecStackId
  -- ^ Stack id env spec
  -- CloudFormation: Expression that resolves to the stack id.
  -- CLI: Reflects stack id
  | EnvSpecStackName
  -- ^ Stack name env spec
  -- CloudFormation: Expression that resolves to the stack name.
  -- CLI: Reflects stack name
  | EnvSpecStackOutput CFT.Output
  -- ^ Output value env spec
  -- CloudFormation: Expression that resolves to the output value.
  -- CLI: Reflects output value from stack
  | EnvSpecStackParameter CFT.Parameter
  -- ^ Parameter value env spec
  -- CloudFormation: Expression that resolves to the parameter value.
  -- CLI: Reflects parameter value from stack
  | EnvSpecStackPrefix Text
  -- ^ Static aws stack name prefixing env spec
  -- CloudFormation: Expression that prefixes a string literal with the stack name.
  -- CLI: Reflects stack name.
  | EnvSpecStatic Text
  -- ^ Static env spec.
  -- CloudFormation: String literal
  -- CLI: Constant return

-- | Construct a ECS task definition environment key value property
-- >>> let envSpecA = StackDeploy.EnvSpec (fromType @"Env-A") (StackDeploy.EnvSpecStatic "Value-A")
-- >>> let envSpecB = StackDeploy.EnvSpec (fromType @"Env-B") StackDeploy.EnvSpecStackName
-- >>> StackDeploy.envSpecEcsTaskDefinitionEnvironment [envSpecB, envSpecA]
-- [KeyValuePairProperty {name = Just (Literal "Env-A"), value = Just (Literal "Value-A")},KeyValuePairProperty {name = Just (Literal "Env-B"), value = Just (Ref "AWS::StackName")}]
envSpecEcsTaskDefinitionEnvironment :: [EnvSpec] -> [ECS.TaskDefinition.KeyValuePairProperty]
envSpecEcsTaskDefinitionEnvironment entries = render <$> List.sortOn (.name) entries
  where
    render (EnvSpec key value) = mkPair key $ renderValue value

    mkPair :: EnvSpecName -> CFT.Value Text -> ECS.TaskDefinition.KeyValuePairProperty
    mkPair key value
      = ECS.TaskDefinition.KeyValuePairProperty
      { name  = pure (CFT.Literal $ convert key)
      , value = pure value
      }

-- | Construct a lambda environment
-- >>> let envSpecA = StackDeploy.EnvSpec (fromType @"Env-A") (StackDeploy.EnvSpecStatic "Value-A")
-- >>> let envSpecB = StackDeploy.EnvSpec (fromType @"Env-B") StackDeploy.EnvSpecStackName
-- >>> StackDeploy.envSpecLambdaEnvironment [envSpecB, envSpecA]
-- EnvironmentProperty {variables = Just (fromList [("Env-A",Literal "Value-A"),("Env-B",Ref "AWS::StackName")])}
envSpecLambdaEnvironment :: [EnvSpec] -> Lambda.Function.EnvironmentProperty
envSpecLambdaEnvironment entries
  = Lambda.Function.mkEnvironmentProperty
  { Lambda.Function.variables = pure variables
  }
  where
    variables :: Map Text (CFT.Value Text)
    variables = fromList $ render <$> List.sortOn (.name) entries

    render (EnvSpec key value) = (convert @Text key, renderValue value)

-- | Construct a posix environment list
--
-- Only intent for running 3rd party binaries via exporting that environment list before
-- spawning a new process.
--
-- If its planned to re-use an environment in the same HS process juse `readEnvSpecFromStack` to read the values as the CF stack would define them.
--
-- >>> let envSpecA = StackDeploy.EnvSpec (fromType @"Env-A") (StackDeploy.EnvSpecStatic "Value-A")
-- >>> let envSpecB = StackDeploy.EnvSpec (fromType @"Env-B") StackDeploy.EnvSpecStackName
-- >>> let epochTime = Time.posixSecondsToUTCTime 0
-- >>> let stack = CF.newStack "test-stack" epochTime CF.StackStatus_UPDATE_COMPLETE
-- >>> envSpec <- StackDeploy.envSpecPosixEnvironment stack [envSpecB, envSpecA]
-- >>> envSpec
-- [("Env-A","Value-A"),("Env-B","test-stack")]
envSpecPosixEnvironment
  :: forall m . MonadIO m
  => CF.Stack
  -> [EnvSpec]
  -> m [(String, String)]
envSpecPosixEnvironment stack = traverse render . List.sortOn (.name)
  where
    render :: EnvSpec -> m (String, String)
    render entry@EnvSpec{..} = (convertVia @Text name,) . convert <$> readEnvSpecFromStack stack entry

-- | Read an env spec value as AWS resources would do
--
-- Primary use case is to initialize operations CLI to the same state an equivalent AWS resource would have.
--
-- >>> let envSpec = StackDeploy.EnvSpec (fromType @"STACK_NAME") StackDeploy.EnvSpecStackName
-- >>> let epochTime = Time.posixSecondsToUTCTime 0
-- >>> let stack = CF.newStack "test-stack" epochTime CF.StackStatus_UPDATE_COMPLETE
-- >>> value <- StackDeploy.readEnvSpecFromStack stack envSpec
-- >>> value
-- "test-stack"
readEnvSpecFromStack :: forall m . MonadIO m => CF.Stack -> EnvSpec -> m Text
readEnvSpecFromStack stack EnvSpec{..} = case value of
  EnvSpecStackId                  -> maybe failAbsentStackId pure stack.stackId
  EnvSpecStackName                -> pure stack.stackName
  EnvSpecStackOutput output       -> liftIO $ StackDeploy.fetchStackOutput stack output
  EnvSpecStackParameter parameter -> fetchParameter stack parameter
  EnvSpecStackPrefix text         -> pure $ stack.stackName <> "-" <> text
  EnvSpecStatic text              -> pure text
  where
    failAbsentStackId :: m a
    failAbsentStackId = throwString $ "Missing stack id: " <> show stack

-- | Read an env spec value from the environment
--
-- This function is intend to be run within lambda or ECS tasks reading the env value the cloud formation resource defined. For operations CLI use `readEnvSpecFromStack` instead.
readEnvSpecFromEnvironment :: EnvSpec -> MIO env Text
readEnvSpecFromEnvironment EnvSpec{..} = convert <$> Environment.getEnv (convertVia @Text name)

renderValue :: EnvSpecValue -> CFT.Value Text
renderValue = \case
  EnvSpecStackId                  -> CFT.awsStackId
  EnvSpecStackName                -> CFT.awsStackName
  EnvSpecStackOutput output       -> output.value
  EnvSpecStackParameter parameter -> CFT.toRef parameter
  EnvSpecStackPrefix value        -> CFT.mkName (CFT.Literal value)
  EnvSpecStatic text              -> CFT.Literal text

fetchParameter
  :: forall m . MonadIO m
  => CF.Stack
  -> CFT.Parameter
  -> m Text
fetchParameter stack stratosphereParameter =
  maybe
    (failParamemterKey "missing")
    (maybe (failParamemterKey "has no value") pure . (.parameterValue))
    $ Foldable.find
      ((==) (pure key) . (.parameterKey))
      (fromMaybe [] stack.parameters)
  where
    key = stratosphereParameter.name

    failParamemterKey :: Text -> m a
    failParamemterKey message
      = failStack
      $ "Parameter " <> convertText key <> " " <> message

    failStack :: Text -> m a
    failStack message
      = throwString
      . convertText
      $ "Stack: " <> stack.stackName <> " " <> message
