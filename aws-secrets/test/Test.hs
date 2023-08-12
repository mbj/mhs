{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import AWS.Secrets
import AWS.Secrets.Prelude

import qualified Devtools
import qualified StackDeploy.Component              as StackDeploy
import qualified StackDeploy.Template               as StackDeploy
import qualified Stratosphere                       as CFT
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
        [internalComponent @TestSecret]

    internalTemplateNoExternal
      = StackDeploy.mkTemplate (fromType @"secrets-internal-no-external")
        [internalComponent @TestSecretNoExternal]
