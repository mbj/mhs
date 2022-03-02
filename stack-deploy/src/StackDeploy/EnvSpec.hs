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

import StackDeploy.Prelude
import StackDeploy.Utils hiding (StackName)
import Stratosphere

import qualified Amazonka.CloudFormation.Types as CF
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Key                as JSON.Key
import qualified Data.Foldable                 as Foldable
import qualified Data.List                     as List
import qualified UnliftIO.Environment          as Environment

data Value
  = StackName
  | StackOutput Output
  | StackParameter Parameter
  | StackPrefix Text
  | Static Text

data Entry = Entry
  { envName  :: Text
  , envValue :: Value
  }

ecsTaskDefinitionEnvironment :: [Entry] -> [ECSTaskDefinitionKeyValuePair]
ecsTaskDefinitionEnvironment entries = render <$> List.sortOn envName entries
  where
    render (Entry key value) = mkPair key $ renderValue value

    mkPair :: Text -> Val Text -> ECSTaskDefinitionKeyValuePair
    mkPair key value
      = ecsTaskDefinitionKeyValuePair
      & ecstdkvpName  ?~ Literal key
      & ecstdkvpValue ?~ value

lambdaEnvironment :: [Entry] -> LambdaFunctionEnvironment
lambdaEnvironment entries = lambdaFunctionEnvironment & lfeVariables ?~ environmentObject
  where
    environmentObject :: JSON.Object
    environmentObject = fromList $ render <$> List.sortOn envName entries

    render (Entry key value) = (JSON.Key.fromText key, JSON.toJSON $ renderValue value)

posixEnv :: CF.Stack -> [Entry] -> RIO env [(String, String)]
posixEnv stack = traverse render . List.sortOn envName
  where
    render :: Entry -> RIO env (String, String)
    render entry@Entry{..} = do
      (convert envName,) . convert <$> loadStack stack entry

loadStack :: CF.Stack -> Entry -> RIO env Text
loadStack stack Entry{..} = case envValue of
  StackOutput output'  -> liftIO $ fetchOutput stack output'
  StackParameter param -> fetchParam stack param
  StackPrefix text     -> pure $ (stack ^. CF.stack_stackName) <> "-" <> text
  StackName            -> pure $ stack ^. CF.stack_stackName
  Static text          -> pure text

loadEnv :: Entry -> RIO env Text
loadEnv Entry{..} = convert <$> Environment.getEnv (convert envName)

renderValue :: Value -> Val Text
renderValue = \case
  StackName            -> awsStackName
  StackOutput output'  -> output' ^. outputValue
  StackParameter param -> toRef param
  StackPrefix value    -> mkName (Literal value)
  Static text          -> Literal text

fetchParam
  :: CF.Stack
  -> Stratosphere.Parameter
  -> RIO env Text
fetchParam stack param =
  maybe
    (failOutputKey "missing")
    (maybe (failOutputKey "has no value") pure . getField @"parameterValue")
    $ Foldable.find
      ((==) (pure key) . getField @"parameterKey")
      (fromMaybe [] $ getField @"parameters" stack)
  where
    key = param ^. Stratosphere.parameterName

    failOutputKey :: Text -> RIO env a
    failOutputKey message
      = failStack
      $ "Parameter " <> convertText key <> " " <> message

    failStack :: Text -> RIO env a
    failStack message
      = throwString
      . convertText
      $ "Stack: " <> (stack ^. CF.stack_stackName) <> " " <> message
