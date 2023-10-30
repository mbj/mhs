{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import AWS.Secrets
import AWS.Secrets.Prelude

import qualified Devtools
import qualified StackDeploy.Component              as StackDeploy
import qualified StackDeploy.EnvSpec                as StackDeploy
import qualified StackDeploy.Template               as StackDeploy
import qualified StackDeploy.Utils                  as StackDeploy
import qualified Stratosphere                       as CFT
import qualified Stratosphere.IAM.Role              as IAM
import qualified Stratosphere.Lambda.Function       as Lambda
import qualified Stratosphere.SecretsManager.Secret as SecretsManager
import qualified Test.Tasty                         as Tasty

data TestSecret
  = TestExternal
  | TestInternal
  deriving stock (Bounded, Enum, Show)

instance IsSecret TestSecret where
  secretConfig = \case
    TestExternal -> External
      $ SecretsManager.mkSecret
      & CFT.set @"Name" (secretNameValue "external" "test")
    TestInternal -> Internal
      $ SecretsManager.mkSecret
      & CFT.set @"Name" (secretNameValue "internal" "test")

data TestSecretNoExternal
  = TestNoExternal
  deriving stock (Bounded, Enum, Show)

instance IsSecret TestSecretNoExternal where
  secretConfig = \case
    TestNoExternal -> Internal
      $ SecretsManager.mkSecret
      & CFT.set @"Name" (secretNameValue "internal" "test")

main :: IO ()
main =
  liftIO . Tasty.defaultMain .
    Tasty.testGroup "aws-secrets" $
      [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "aws-secrets"])
      , StackDeploy.testTree
          [ internalTemplate
          , internalTemplateNoExternal
          , externalTemplate @TestSecret
          ]
      ]
  where
    internalTemplate
      = StackDeploy.mkTemplate (fromType @"secrets-internal")
        [ internalComponent @TestSecret
        , lambdaComponent
        ]

    internalTemplateNoExternal
      = StackDeploy.mkTemplate (fromType @"secrets-internal-no-external")
        [internalComponent @TestSecretNoExternal]

    lambdaComponent = mempty
      { StackDeploy.resources = [lambdaFunction, lambdaRole] }
      where
        lambdaFunction
          = CFT.resource "TestLambdaFunction"
          $ Lambda.mkFunction lambdaFunctionCode (StackDeploy.getAttArn lambdaRole)
          & CFT.set @"Environment"
             (StackDeploy.lambdaEnvironment $ envSpecEntries [TestExternal, TestInternal])

        lambdaRole
          = CFT.resource "TestLambdaRole"
          $ IAM.mkRole (StackDeploy.assumeRole "lambda.amazonaws.com")
          & CFT.set @"Policies" [getSecretValuePolicy [TestExternal, TestInternal]]

        lambdaFunctionCode :: Lambda.CodeProperty
        lambdaFunctionCode
          = Lambda.mkCodeProperty
          & CFT.set @"S3Bucket" "test-bucket"
          & CFT.set @"S3Key"    "test-key"
