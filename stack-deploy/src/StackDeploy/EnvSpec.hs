module StackDeploy.EnvSpec where

import StackDeploy.Prelude
import StackDeploy.Utils
import Stratosphere

import qualified Data.Foldable                    as Foldable
import qualified Data.List                        as List
import qualified Network.AWS.CloudFormation.Types as CF

data Value
  = StackOutput Output
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

    renderValue = \case
      StackOutput output'  -> output' ^. outputValue
      StackParameter param -> toRef param
      StackPrefix value    -> mkName (Literal value)
      Static text          -> Literal text

    mkPair :: Text -> Val Text -> ECSTaskDefinitionKeyValuePair
    mkPair key value
      = ecsTaskDefinitionKeyValuePair
      & ecstdkvpName  ?~ Literal key
      & ecstdkvpValue ?~ value

posixEnv :: CF.Stack -> [Entry] -> RIO env [(String, String)]
posixEnv stack = traverse render . List.sortOn envName
  where
    render :: Entry -> RIO env (String, String)
    render (Entry key value) = do
      (convert key,) . convert <$> renderValue value

    renderValue :: Value -> RIO env Text
    renderValue = \case
      StackOutput output'  -> liftIO $ fetchOutput stack output'
      StackParameter param -> fetchParam stack param
      StackPrefix text     -> pure $ (stack ^. CF.sStackName) <> "-" <> text
      Static text          -> pure text

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
