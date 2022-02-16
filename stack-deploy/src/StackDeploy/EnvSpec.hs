module StackDeploy.EnvSpec
  ( Entry(..)
  , Value(..)
  , ecsTaskDefinitionEnvironment
  , lambdaEnvironment
  , loadStackValue
  , posixEnv
  )
where

import StackDeploy.Prelude
import StackDeploy.Utils hiding (StackName)
import Stratosphere

import qualified Data.Aeson                       as JSON
import qualified Data.Foldable                    as Foldable
import qualified Data.List                        as List
import qualified Network.AWS.CloudFormation.Types as CF

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

    render (Entry key value) = (key, JSON.toJSON $ renderValue value)

posixEnv :: CF.Stack -> [Entry] -> RIO env [(String, String)]
posixEnv stack = traverse render . List.sortOn envName
  where
    render :: Entry -> RIO env (String, String)
    render (Entry key value) = do
      (convert key,) . convert <$> loadStackValue stack value

loadStackValue :: CF.Stack -> Value -> RIO env Text
loadStackValue stack = \case
  StackOutput output'  -> liftIO $ fetchOutput stack output'
  StackParameter param -> fetchParam stack param
  StackPrefix text     -> pure $ (stack ^. CF.sStackName) <> "-" <> text
  StackName            -> pure $ stack ^. CF.sStackName
  Static text          -> pure text

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
    (maybe (failOutputKey "has no value") pure . view CF.pParameterValue)
    $ Foldable.find
      ((==) (pure key) . view CF.pParameterKey)
      (view CF.sParameters stack)
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
      $ "Stack: " <> (stack ^. CF.sStackName) <> " " <> message
