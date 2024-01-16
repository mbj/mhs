module StackDeploy.CLI.Utils
  ( exitCode
  , failure
  , instanceNameOption
  , success
  , templateNameOption
  , tokenOption
  )
where

import GHC.TypeLits (KnownSymbol)
import Options.Applicative
import StackDeploy.IO
import StackDeploy.Prelude
import StackDeploy.Types

import qualified StackDeploy.InstanceSpec  as StackDeploy
import qualified StackDeploy.NamedTemplate as StackDeploy
import qualified System.Exit               as System

instanceNameOption :: Parser StackDeploy.InstanceName
instanceNameOption =
  option
    reader
    (long "instance" <> metavar "INSTANCE" <> help "Stack instance name")

templateNameOption :: Parser StackDeploy.TemplateName
templateNameOption =
  option
    reader
    (long "template" <> metavar "TEMPLATE_NAME" <> help "Template name")

tokenOption :: Parser Token
tokenOption =
  option
    reader
    (long "token" <> metavar "TOKEN" <> help "Stack update token")

reader :: KnownSymbol a => ReadM (BoundText a)
reader = maybeReader (convertMaybe . convert @Text)

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
