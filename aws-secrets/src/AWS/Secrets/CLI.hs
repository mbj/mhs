{-# LANGUAGE AllowAmbiguousTypes #-}

module AWS.Secrets.CLI (parserInfo) where

import AWS.Secrets
import AWS.Secrets.Prelude
import CLI.Utils

import qualified Data.Aeson            as JSON
import qualified Data.Foldable         as Foldable
import qualified Data.Text.IO          as IO
import qualified Options.Applicative   as CLI
import qualified StackDeploy.CLI.Utils as StackDeploy.CLI
import qualified StackDeploy.Stack     as StackDeploy
import qualified System.Exit           as System
import qualified UnliftIO.Exception    as UnliftIO

parserInfo :: forall a env . (Env env, IsSecret a) => CLI.ParserInfo (MIO env System.ExitCode)
parserInfo = CLI.info (CLI.helper <*> subcommands) CLI.idm
  where
    subcommands
      =  CLI.hsubparser
      $ mkCommand
        "list"
        (pure $ list $> System.ExitSuccess)
        "list secrets"
      <> mkCommand
        "secret-value"
        (printStackSecret readStackSecretValue)
        "print secret"
      <> mkCommand
        "secret-arn"
        (printStackSecret fetchStackSecretArn)
        "print secret ARN"

    secretNameOption :: CLI.Parser a
    secretNameOption = CLI.option readSecret (CLI.long "secret-name" <> CLI.metavar "SECRET_NAME")

    readSecret = do
      value <- convert @Text <$> CLI.str @Text
      maybe
        (fail $ "Unknown secret name: " <> convert value)
        pure
        (Foldable.find ((value ==) . snakeCase) (secrets @a))

    list  = traverse_ (liftIO . IO.putStrLn . snakeCase) (secrets @a)

    printStackSecret action =
      evaluate <$> StackDeploy.CLI.instanceNameOption <*> secretNameOption
      where
        evaluate instanceName secret = do
          maybe absent present =<< StackDeploy.readCloudFormationStack instanceName
          where
            absent = UnliftIO.throwString $ "Stack does not exist: " <> convertVia @Text instanceName
            present stack = do
              putStrLn =<< action stack secret
              pure System.ExitSuccess

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . IO.putStrLn

snakeCase :: IsSecret a => a -> Text
snakeCase = convert . JSON.camelTo2 '_' . show
