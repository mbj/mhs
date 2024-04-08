module StackDeploy.CLI.Utils
  ( exitCode
  , failure
  , instanceNameOption
  , success
  , templateNameOption
  , tokenOption
  , withExistingStack
  )
where

import GHC.TypeLits (KnownSymbol)
import StackDeploy.IO
import StackDeploy.Prelude
import StackDeploy.Types

import qualified MIO.Amazonka              as AWS
import qualified Options.Applicative       as CLI
import qualified StackDeploy.InstanceSpec  as StackDeploy
import qualified StackDeploy.NamedTemplate as StackDeploy
import qualified StackDeploy.Stack         as StackDeploy
import qualified System.Exit               as System

instanceNameOption :: CLI.Parser StackDeploy.InstanceName
instanceNameOption =
  CLI.option
    reader
    (CLI.long "instance" <> CLI.metavar "INSTANCE" <> CLI.help "Stack instance name")

templateNameOption :: CLI.Parser StackDeploy.TemplateName
templateNameOption =
  CLI.option
    reader
    (CLI.long "template" <> CLI.metavar "TEMPLATE_NAME" <> CLI.help "Template name")

tokenOption :: CLI.Parser Token
tokenOption =
  CLI.option
    reader
    (CLI.long "token" <> CLI.metavar "TOKEN" <> CLI.help "Stack update token")

reader :: KnownSymbol a => CLI.ReadM (BoundText a)
reader = CLI.maybeReader (convertMaybe . convert @Text)

{- HLINT ignore "Unnecessarily monadic" -}
success :: MIO env System.ExitCode
success = pure System.ExitSuccess

failure :: Text -> MIO env System.ExitCode
failure message = do
  say message
  pure $ System.ExitFailure 1

exitCode :: RemoteOperationResult -> MIO env System.ExitCode
exitCode = \case
  RemoteOperationSuccess -> success
  RemoteOperationFailure -> failure "Stack operation failed"

withExistingStack
  :: AWS.Env env
  => StackDeploy.InstanceName
  -> (ExistingStack -> MIO env System.ExitCode)
  -> MIO env System.ExitCode
withExistingStack instanceName action =
  StackDeploy.readExistingStack instanceName >>=
    maybe
      (failure $ "Stack does not exist: " <> convert instanceName)
      action
