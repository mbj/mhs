module StackDeploy.EnvSpec
  ( Entry(..)
  , Value(..)
  , ecsTaskDefinitionEnvironment
  , lambdaEnvironment
  , loadEnv
  , loadStack
  , posixEnv
  )
where

import Data.Map.Strict (Map)
import StackDeploy.Prelude
import StackDeploy.Utils hiding (StackName)

import qualified Amazonka.CloudFormation.Types   as CF
import qualified Data.Foldable                   as Foldable
import qualified Data.List                       as List
import qualified Stratosphere                    as CFT
import qualified Stratosphere.ECS.TaskDefinition as ECS.TaskDefinition
import qualified Stratosphere.Lambda.Function    as Lambda.Function
import qualified UnliftIO.Environment            as Environment

data Value
  = StackName
  | StackOutput CFT.Output
  | StackParameter CFT.Parameter
  | StackPrefix Text
  | Static Text

data Entry = Entry
  { envName  :: Text
  , envValue :: Value
  }

ecsTaskDefinitionEnvironment :: [Entry] -> [ECS.TaskDefinition.KeyValuePairProperty]
ecsTaskDefinitionEnvironment entries = render <$> List.sortOn (.envName) entries
  where
    render (Entry key value) = mkPair key $ renderValue value

    mkPair :: Text -> CFT.Value Text -> ECS.TaskDefinition.KeyValuePairProperty
    mkPair key value
      = ECS.TaskDefinition.KeyValuePairProperty
      { name  = pure (CFT.Literal key)
      , value = pure value
      }

lambdaEnvironment :: [Entry] -> Lambda.Function.EnvironmentProperty
lambdaEnvironment entries
  = Lambda.Function.mkEnvironmentProperty
  { Lambda.Function.variables = pure variables
  }
  where
    variables :: Map Text (CFT.Value Text)
    variables = fromList $ render <$> List.sortOn (.envName) entries

    render (Entry key value) = (key, renderValue value)

posixEnv :: CF.Stack -> [Entry] -> RIO env [(String, String)]
posixEnv stack = traverse render . List.sortOn (.envName)
  where
    render :: Entry -> RIO env (String, String)
    render entry@Entry{..} = do
      (convert envName,) . convert <$> loadStack stack entry

loadStack :: CF.Stack -> Entry -> RIO env Text
loadStack stack Entry{..} = case envValue of
  StackOutput output'  -> liftIO $ fetchOutput stack output'
  StackParameter param -> fetchParam stack param
  StackPrefix text     -> pure $ stack.stackName <> "-" <> text
  StackName            -> pure $ stack.stackName
  Static text          -> pure text

loadEnv :: Entry -> RIO env Text
loadEnv Entry{..} = convert <$> Environment.getEnv (convert envName)

renderValue :: Value -> CFT.Value Text
renderValue = \case
  StackName            -> awsStackName
  StackOutput output'  -> (output'.value)
  StackParameter param -> CFT.toRef param
  StackPrefix value    -> mkName (CFT.Literal value)
  Static text          -> CFT.Literal text

fetchParam
  :: CF.Stack
  -> CFT.Parameter
  -> RIO env Text
fetchParam stack stratosphereParameter =
  maybe
    (failOutputKey "missing")
    (maybe (failOutputKey "has no value") pure . getField @"parameterValue")
    $ Foldable.find
      ((==) (pure key) . getField @"parameterKey")
      (fromMaybe [] $ getField @"parameters" stack)
  where
    key = stratosphereParameter.name

    failOutputKey :: Text -> RIO env a
    failOutputKey message
      = failStack
      $ "Parameter " <> convertText key <> " " <> message

    failStack :: Text -> RIO env a
    failStack message
      = throwString
      . convertText
      $ "Stack: " <> stack.stackName <> " " <> message
