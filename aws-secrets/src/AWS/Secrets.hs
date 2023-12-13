{-# LANGUAGE AllowAmbiguousTypes #-}

module AWS.Secrets
  ( Env
  , IsSecret(..)
  , SecretConfig(..)
  , arnValue
  , envSpec
  , externalTemplate
  , fetchStackSecretArn
  , getSecretValuePolicy
  , internalComponent
  , rdsGeneratePostgresqlPassword
  , readEnvSecretValue
  , readStackSecretValue
  , secretNameValue
  , secrets
  )
where

import AWS.Secrets.Prelude

import qualified Amazonka
import qualified Amazonka.CloudFormation.Types          as CF
import qualified Amazonka.SecretsManager.GetSecretValue as SecretsManager
import qualified Data.Aeson                             as JSON
import qualified Data.List                              as List
import qualified Data.Text                              as Text
import qualified MIO.Amazonka                           as AWS
import qualified MIO.Log                                as Log
import qualified StackDeploy.Component                  as StackDeploy
import qualified StackDeploy.EnvSpec                    as StackDeploy
import qualified StackDeploy.NamedTemplate              as StackDeploy
import qualified StackDeploy.Stack                      as StackDeploy
import qualified Stratosphere                           as CFT
import qualified Stratosphere.IAM.Role                  as IAM.Role
import qualified Stratosphere.SecretsManager.Secret     as SecretsManager
import qualified UnliftIO.Environment                   as Environment
import qualified UnliftIO.Exception                     as UnliftIO

type Env env = (AWS.Env env, Log.Env env)

data SecretConfig
  = Internal SecretsManager.Secret
  | External SecretsManager.Secret

class (Bounded a, Enum a, Show a) => IsSecret a where
  secretConfig :: a -> SecretConfig

secrets :: IsSecret a => [a]
secrets = [minBound..]

externalSecretsStackParameter :: CFT.Parameter
externalSecretsStackParameter = CFT.mkParameter "ExternalSecretsStack" "String"

internalComponent :: forall a . IsSecret a => StackDeploy.Component
internalComponent = mempty
  { StackDeploy.outputs    = CFT.Outputs $ internalOutputs <> externalOutputs
  , StackDeploy.resources  = CFT.Resources $ mkSecretResource <$> internalSecrets
  , StackDeploy.parameters = CFT.Parameters effectiveParameters
  }
  where
    internalOutputs = mkInternalOutput         <$> internalSecrets
    externalOutputs = mkInternalExternalOutput <$> externalSecrets

    effectiveParameters =
      [externalSecretsStackParameter | not $ List.null externalOutputs]

    (externalSecrets, internalSecrets) = partitionSecrets @a

externalTemplate
  :: forall a . IsSecret a
  => StackDeploy.NamedTemplate
externalTemplate
  = StackDeploy.mkNamedTemplate (fromType @"secrets-external")
  $ (CFT.mkTemplate . CFT.Resources $ mkSecretResource <$> externalSecrets)
    { CFT.outputs = pure . CFT.Outputs $ mkExternalOutput <$> externalSecrets }
  where
    mkExternalOutput :: a -> CFT.Output
    mkExternalOutput secret = (mkInternalOutput secret)
      { CFT.export
         = pure
         . CFT.OutputExport
         $ mkOutputExportNameValue CFT.awsStackName secret
      }

    (externalSecrets, _internalSecrets) = partitionSecrets @a

mkInternalExternalOutput :: IsSecret a => a -> CFT.Output
mkInternalExternalOutput secret
  = CFT.mkOutput (arnOutputName secret) (arnExternalValue secret)

mkInternalOutput :: IsSecret a => a -> CFT.Output
mkInternalOutput secret = CFT.mkOutput (arnOutputName secret) (arnInternalValue secret)

partitionSecrets :: forall a . IsSecret a => ([a], [a])
partitionSecrets = List.partition (filter . secretConfig) $ secrets @a
  where
    filter = \case
      Internal{} -> False
      External{} -> True

logicalResourceName :: IsSecret a => a -> Text
logicalResourceName = (<> "Secret") . convert . show

arnValue :: IsSecret a => a -> CFT.Value Text
arnValue secret = case secretConfig secret of
  Internal{} -> arnInternalValue secret
  External{} -> arnExternalValue secret

arnExternalValue :: IsSecret a => a -> CFT.Value Text
arnExternalValue = CFT.ImportValue . mkOutputExportNameValue (CFT.toRef externalSecretsStackParameter)

arnInternalValue :: IsSecret a => a -> CFT.Value Text
arnInternalValue secret = CFT.Ref $ logicalResourceName secret

arnOutputName :: IsSecret a => a -> Text
arnOutputName secret = logicalResourceName secret <> "Arn"

mkOutputExportNameValue :: IsSecret a => CFT.Value Text -> a -> CFT.Value Text
mkOutputExportNameValue stackName secret
  = CFT.Join "-" [stackName, CFT.Literal $ arnOutputName secret]

mkSecretResource :: IsSecret a => a -> CFT.Resource
mkSecretResource secret
  = CFT.resource (logicalResourceName secret)
  . extractSecretsManagerSecret
  $ secretConfig secret
  where
    extractSecretsManagerSecret = \case
      (Internal value) -> value
      (External value) -> value

secretNameValue :: Text -> CFT.Value Text -> CFT.Value Text
secretNameValue namespace name
  = CFT.Join "/"
  [ CFT.awsStackName
  , CFT.Literal namespace
  , name
  ]

fetchStackSecretArn :: IsSecret a => CF.Stack -> a -> MIO env Text
fetchStackSecretArn stack
  = liftIO . StackDeploy.fetchStackOutput stack . mkInternalOutput

readStackSecretValue :: Env env => IsSecret a => CF.Stack -> a -> MIO env Text
readStackSecretValue stack secret = readSecretsManagerSecret =<< fetchStackSecretArn stack secret

readEnvSecretValue :: (Env env, IsSecret a) => a -> MIO env Text
readEnvSecretValue =
  readSecretsManagerSecret . convert <=< Environment.getEnv . convertVia @Text . envSpecName

readSecretsManagerSecret :: Env env => Text -> MIO env Text
readSecretsManagerSecret arn = do
  Log.info $ "Reading secret: " <> arn

  AWS.send (SecretsManager.newGetSecretValue $ convertText arn)
    >>= decode . fmap Amazonka.fromSensitive . (.secretString)
  where
    decode :: Maybe Text -> MIO env Text
    decode = maybe
      (UnliftIO.throwString . convert $ "Secrets manager returned " <> arn <> " secret without secret string")
      pure

rdsGeneratePostgresqlPassword :: SecretsManager.GenerateSecretStringProperty
rdsGeneratePostgresqlPassword
  = SecretsManager.mkGenerateSecretStringProperty
  & CFT.set @"ExcludeCharacters" "/\"@"
  & CFT.set @"PasswordLength"    (CFT.Literal 48)

envSpecName :: IsSecret a => a -> StackDeploy.EnvSpecName
envSpecName = convertImpure . (<> "_ARN") . Text.toUpper . convert . JSON.camelTo2 '_' . show

envSpec :: IsSecret a => [a] -> [StackDeploy.EnvSpec]
envSpec = fmap $ \secret ->
  StackDeploy.EnvSpec
  { name  = envSpecName secret
  , value = StackDeploy.EnvSpecStackOutput $ case secretConfig secret of
      Internal{} -> mkInternalOutput secret
      External{} -> mkInternalExternalOutput secret
  }

getSecretValuePolicy :: IsSecret a => [a] -> IAM.Role.PolicyProperty
getSecretValuePolicy values = IAM.Role.mkPolicyProperty [("Statement", statement)] "allow-secrets"
  where
    statement :: JSON.Value
    statement = JSON.Object
      [ ("Action",   "secretsmanager:GetSecretValue")
      , ("Effect",   "Allow")
      , ("Resource", JSON.toJSON $ arnValue <$> values)
      ]
