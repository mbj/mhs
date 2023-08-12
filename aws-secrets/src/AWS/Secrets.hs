{-# LANGUAGE AllowAmbiguousTypes #-}

module AWS.Secrets
  ( Env
  , IsSecret(..)
  , SecretConfig(..)
  , arnValue
  , externalTemplate
  , fetchStackSecretArn
  , internalComponent
  , rdsGeneratePostgresqlPassword
  , readStackSecretValue
  , secretNameValue
  , secrets
  )
where

import AWS.Secrets.Prelude

import qualified Amazonka
import qualified Amazonka.CloudFormation.Types          as CF
import qualified Amazonka.SecretsManager.GetSecretValue as SecretsManager
import qualified Data.List                              as List
import qualified MIO.Amazonka                           as AWS
import qualified MIO.Log                                as Log
import qualified StackDeploy.Component                  as StackDeploy
import qualified StackDeploy.Template                   as StackDeploy
import qualified StackDeploy.Utils
import qualified Stratosphere                           as CFT
import qualified Stratosphere.SecretsManager.Secret     as SecretsManager
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
  , StackDeploy.resources  = CFT.Resources $ mkResource <$> internalSecrets
  , StackDeploy.parameters = CFT.Parameters effectiveParameters
  }
  where
    internalOutputs = mkInternalOutput <$> internalSecrets
    externalOutputs = mkExternalOutput <$> externalSecrets @a

    effectiveParameters =
      [externalSecretsStackParameter | not $ List.null externalOutputs]

    mkExternalOutput :: a -> CFT.Output
    mkExternalOutput secret
      = CFT.mkOutput (arnOutputName secret) (arnExternalValue secret)

    internalSecrets = filterSecrets @a $ \case
      Internal{} -> True
      External{} -> False

externalTemplate
  :: forall a . IsSecret a
  => StackDeploy.Template
externalTemplate =
  StackDeploy.Template
    { name = fromType @"secrets-external"
    , ..
    }
  where
    stratosphere :: CFT.Template
    stratosphere =
      (CFT.mkTemplate . CFT.Resources $ mkResource <$> externalSecrets @a)
        { CFT.outputs = pure . CFT.Outputs $ mkExternalOutput <$> externalSecrets }

    mkExternalOutput :: a -> CFT.Output
    mkExternalOutput secret = (mkInternalOutput secret)
      { CFT.export
         = pure
         . CFT.OutputExport
         $ mkOutputExportNameValue CFT.awsStackName secret
      }

externalSecrets :: forall a . IsSecret a => [a]
externalSecrets = filterSecrets @a $ \case
  Internal{} -> False
  External{} -> True

filterSecrets :: forall a . IsSecret a => (SecretConfig -> Bool) -> [a]
filterSecrets filter = List.filter (filter . secretConfig) $ secrets @a

extractSecretsManagerSecret :: SecretConfig -> SecretsManager.Secret
extractSecretsManagerSecret = \case
  (Internal secret) -> secret
  (External secret) -> secret

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

mkResource :: IsSecret a => a -> CFT.Resource
mkResource secret
  = CFT.resource (logicalResourceName secret)
  . extractSecretsManagerSecret
  $ secretConfig secret

mkInternalOutput :: IsSecret a => a -> CFT.Output
mkInternalOutput secret = CFT.mkOutput (arnOutputName secret) (arnInternalValue secret)

secretNameValue :: Text -> CFT.Value Text -> CFT.Value Text
secretNameValue namespace name
  = CFT.Join "/"
  [ CFT.awsStackName
  , CFT.Literal namespace
  , name
  ]

fetchStackSecretArn :: IsSecret a => CF.Stack -> a -> MIO env Text
fetchStackSecretArn stack
  = liftIO . StackDeploy.Utils.fetchOutput stack . mkInternalOutput

readStackSecretValue :: Env env => IsSecret a => CF.Stack -> a -> MIO env Text
readStackSecretValue stack secret = readSecretsManagerSecret =<< fetchStackSecretArn stack secret

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
