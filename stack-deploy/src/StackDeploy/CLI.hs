module StackDeploy.CLI (parserInfo) where

import Control.Lens ((.~))
import Data.Conduit ((.|), runConduit)
import Options.Applicative hiding (value)
import StackDeploy.CLI.Utils
import StackDeploy.Events
import StackDeploy.IO
import StackDeploy.Parameters
import StackDeploy.Prelude
import StackDeploy.Stack
import StackDeploy.Types
import StackDeploy.Wait
import System.Exit (ExitCode(..))

import qualified Amazonka.CloudFormation.CancelUpdateStack   as CF
import qualified Amazonka.CloudFormation.DescribeStackEvents as CF
import qualified Amazonka.CloudFormation.Types               as CF
import qualified Data.Attoparsec.Text                        as Text
import qualified Data.ByteString.Lazy                        as LBS
import qualified Data.Char                                   as Char
import qualified Data.Conduit.Combinators                    as Conduit
import qualified Data.Text.Encoding                          as Text
import qualified Data.Text.IO                                as Text
import qualified MIO.Amazonka                                as AWS
import qualified StackDeploy.AWS                             as AWS
import qualified StackDeploy.Env                             as StackDeploy
import qualified StackDeploy.InstanceSpec                    as InstanceSpec
import qualified StackDeploy.Template                        as Template

parserInfo
  :: forall env . (AWS.Env env, StackDeploy.Env env)
  => InstanceSpec.Provider env
  -> ParserInfo (MIO env ExitCode)
parserInfo instanceSpecProvider = wrapHelper commands "stack commands"
  where
    commands :: Parser (MIO env ExitCode)
    commands = hsubparser
      $  mkCommand "instance" instanceCommands     "instance commands"
      <> mkCommand "spec"     specCommands         "instance spec commands"
      <> mkCommand "token"    (pure printNewToken) "print a new stack token"
      <> mkCommand "template" templateCommands     "template commands"

    instanceCommands :: Parser (MIO env ExitCode)
    instanceCommands = hsubparser
      $  mkCommand "cancel"   (cancel <$> instanceSpecNameOption)                "cancel stack update"
      <> mkCommand "create"   (create <$> instanceSpecNameOption <*> parameters) "create stack"
      <> mkCommand "delete"   (delete <$> instanceSpecNameOption)                "delete stack"
      <> mkCommand "events"   (events <$> instanceSpecNameOption)                "list stack events"
      <> mkCommand "list"     (pure list)                                        "list stack instances"
      <> mkCommand "outputs"  (outputs <$> instanceSpecNameOption)               "list stack outputs"
      <> mkCommand "sync"     (sync <$> instanceSpecNameOption <*> parameters)   "sync stack with spec"
      <> mkCommand "update"   (update <$> instanceSpecNameOption <*> parameters) "update existing stack"
      <> mkCommand "wait"     (wait <$> instanceSpecNameOption <*> tokenParser)  "wait for stack operation"
      <> mkCommand "watch"    (watch <$> instanceSpecNameOption)                 "watch stack events"

    templateCommands :: Parser (MIO env ExitCode)
    templateCommands = hsubparser
      $  mkCommand "list"    (pure listTemplates)              "list templates"
      <> mkCommand "render"  (render <$> templateNameArgument) "render template"

    specCommands :: Parser (MIO env ExitCode)
    specCommands = hsubparser
      $  mkCommand "list"   (pure listSpecs) "list stack specifications"

    tokenParser :: Parser Token
    tokenParser = Token <$> argument str (metavar "TOKEN")

    mkCommand :: String -> Parser b -> String -> Mod CommandFields b
    mkCommand name parser desc = command name (wrapHelper parser desc)

    wrapHelper :: Parser b -> String -> ParserInfo b
    wrapHelper parser desc = info parser (progDesc desc)

    cancel :: InstanceSpec.Name env -> MIO env ExitCode
    cancel name = do
      void . AWS.send . CF.newCancelUpdateStack $ toText name
      success

    create :: InstanceSpec.Name env -> Parameters -> MIO env ExitCode
    create name params = do
      spec <- InstanceSpec.get instanceSpecProvider name params
      exitCode =<< perform (OpCreate spec)

    update :: InstanceSpec.Name env -> Parameters -> MIO env ExitCode
    update name params = do
      spec     <- InstanceSpec.get instanceSpecProvider name params
      stackId  <- getExistingStackId name

      exitCode =<< perform (OpUpdate stackId spec)

    sync :: InstanceSpec.Name env -> Parameters -> MIO env ExitCode
    sync name params = do
      spec <- InstanceSpec.get instanceSpecProvider name params

      exitCode
        =<< perform . maybe (OpCreate spec) (`OpUpdate` spec)
        =<< getStackId name

    wait :: InstanceSpec.Name env -> Token -> MIO env ExitCode
    wait name token = maybe success (waitForOperation token) =<< getStackId name

    outputs :: InstanceSpec.Name env -> MIO env ExitCode
    outputs name = do
      traverse_ printOutput . fromMaybe [] . (.outputs) =<< getExistingStack name
      success
      where
        printOutput :: CF.Output -> MIO env ()
        printOutput = liftIO . Text.putStrLn . convertText . show

    delete :: InstanceSpec.Name env -> MIO env ExitCode
    delete = maybe success (exitCode <=< perform . OpDelete) <=< getStackId

    list :: MIO env ExitCode
    list = do
      runConduit $ stackNames .| Conduit.mapM_ say
      success

    listTemplates :: MIO env ExitCode
    listTemplates = do
      traverse_
        (liftIO . Text.putStrLn . toText . (.name))
        (toList templateProvider)
      success

    listSpecs :: MIO env ExitCode
    listSpecs = do
      traverse_
        (liftIO . Text.putStrLn . toText . (.name))
        (toList instanceSpecProvider)
      success

    events :: InstanceSpec.Name env -> MIO env ExitCode
    events name = do
      runConduit $ AWS.listResource req (fromMaybe [] . (.stackEvents)) .| Conduit.mapM_ printEvent
      success
      where
        req = CF.newDescribeStackEvents & CF.describeStackEvents_stackName .~ pure (toText name)

    watch :: InstanceSpec.Name env -> MIO env ExitCode
    watch name = do
      stackId <- getExistingStackId name
      void $ pollEvents (defaultPoll stackId) printEvent
      success

    waitForOperation :: Token -> Id -> MIO env ExitCode
    waitForOperation token stackId =
      exitCode =<< waitForAccept RemoteOperation{..} printEvent

    printNewToken :: MIO env ExitCode
    printNewToken = do
      say =<< newToken
      success

    render :: Template.Name -> MIO env ExitCode
    render name = do
      template <- Template.get templateProvider name
      say . Text.decodeUtf8 . LBS.toStrict $ Template.encode template
      success

    success :: MIO env ExitCode
    success = pure ExitSuccess

    exitCode = \case
      RemoteOperationSuccess -> success
      RemoteOperationFailure -> pure $ ExitFailure 1

    templateProvider = InstanceSpec.templateProvider instanceSpecProvider

parameter :: Parser Parameter
parameter = option
  parameterReader
  (long "parameter" <> help "Set stack parameter")

parameterReader :: ReadM Parameter
parameterReader = eitherReader (Text.parseOnly parser . convertText)
  where
    parser = do
      name <- ParameterName . convertText <$> Text.many1 (Text.satisfy allowChar)
      Text.skip (== ':')
      value <- ParameterValue . convertText <$> Text.many' Text.anyChar
      void Text.endOfInput

      pure $ Parameter name value

    allowChar = \case
      '-'  -> True
      char -> Char.isDigit char || Char.isAlpha char

parameters :: Parser Parameters
parameters = fromList <$> many parameter
