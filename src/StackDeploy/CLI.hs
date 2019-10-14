module StackDeploy.CLI (parserInfo, stackName) where

import Control.Applicative (many)
import Control.Lens ((&), (.~), view)
import Control.Monad ((<=<), mapM_)
import Data.Conduit ((.|), runConduit)
import Data.String (String)
import Options.Applicative hiding (value)
import StackDeploy.AWS
import StackDeploy.Events
import StackDeploy.IO
import StackDeploy.Parameters
import StackDeploy.Prelude
import StackDeploy.Stack
import StackDeploy.Types
import StackDeploy.Wait
import System.Exit (ExitCode(..))

import qualified Data.Attoparsec.Text                           as Text
import qualified Data.ByteString.Lazy                           as LBS
import qualified Data.Char                                      as Char
import qualified Data.Conduit.Combinators                       as Conduit
import qualified Data.Foldable                                  as Foldable
import qualified Data.Text.Encoding                             as Text
import qualified Data.Text.IO                                   as Text
import qualified Network.AWS                                    as AWS
import qualified Network.AWS.CloudFormation.CancelUpdateStack   as CF
import qualified Network.AWS.CloudFormation.DescribeStackEvents as CF
import qualified Network.AWS.CloudFormation.Types               as CF
import qualified StackDeploy.InstanceSpec                       as InstanceSpec
import qualified StackDeploy.Parameters                         as Parameters
import qualified StackDeploy.Template                           as Template

parserInfo
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => Template.Provider
  -> InstanceSpec.Provider
  -> ParserInfo (m ExitCode)
parserInfo templateProvider instanceSpecProvider = wrapHelper commands "stack commands"
  where
    commands :: Parser (m ExitCode)
    commands = hsubparser
      $  mkCommand "instance" instanceCommands     "instance commands"
      <> mkCommand "spec"     specCommands         "instance spec commands"
      <> mkCommand "token"    (pure printNewToken) "print a new stack token"
      <> mkCommand "template" templateCommands     "template commands"

    instanceCommands :: Parser (m ExitCode)
    instanceCommands = hsubparser
      $  mkCommand "cancel"   (cancel <$> stackName)                "cancel stack update"
      <> mkCommand "create"   (create <$> stackName <*> parameters) "create stack"
      <> mkCommand "delete"   (delete <$> stackName)                "delete stack"
      <> mkCommand "events"   (events <$> stackName)                "list stack events"
      <> mkCommand "list"     (pure list)                           "list stack instances"
      <> mkCommand "outputs"  (outputs <$> stackName)               "list stack outputs"
      <> mkCommand "sync"     (sync <$> stackName <*> parameters)   "sync stack with spec"
      <> mkCommand "update"   (update <$> stackName <*> parameters) "update existing stack"
      <> mkCommand "wait"     (wait <$> stackName <*> tokenParser)  "wait for stack operation"
      <> mkCommand "watch"    (watch <$> stackName)                 "watch stack events"

    templateCommands :: Parser (m ExitCode)
    templateCommands = hsubparser
      $  mkCommand "list"    (pure listTemplates)      "list templates"
      <> mkCommand "render"  (render <$> templateName) "render template"

    specCommands :: Parser (m ExitCode)
    specCommands = hsubparser
      $  mkCommand "list"   (pure listSpecs) "list stack specifications"

    tokenParser :: Parser Token
    tokenParser = Token <$> argument str (metavar "TOKEN")

    mkCommand :: String -> Parser b -> String -> Mod CommandFields b
    mkCommand name parser desc = command name (wrapHelper parser desc)

    wrapHelper :: Parser b -> String -> ParserInfo b
    wrapHelper parser desc = info parser (progDesc desc)

    cancel :: InstanceSpec.Name -> m ExitCode
    cancel name = do
      void . AWS.send . CF.cancelUpdateStack $ toText name
      success

    create :: InstanceSpec.Name -> Parameters -> m ExitCode
    create name params = do
      spec <- InstanceSpec.get instanceSpecProvider name params
      exitCode =<< perform (OpCreate spec)

    update :: InstanceSpec.Name -> Parameters -> m ExitCode
    update name params = do
      spec     <- InstanceSpec.get instanceSpecProvider name params
      stackId  <- getExistingStackId name

      exitCode =<< perform (OpUpdate stackId spec)

    sync :: InstanceSpec.Name -> Parameters -> m ExitCode
    sync name params = do
      spec <- InstanceSpec.get instanceSpecProvider name params

      exitCode
        =<< perform . maybe (OpCreate spec) (`OpUpdate` spec)
        =<< getStackId name

    wait :: InstanceSpec.Name -> Token -> m ExitCode
    wait name token = maybe success (waitForOperation token) =<< getStackId name

    outputs :: InstanceSpec.Name -> m ExitCode
    outputs name = do
      mapM_ printOutput =<< (view CF.sOutputs <$> getExistingStack name)
      success
      where
        printOutput :: CF.Output -> m ()
        printOutput = liftIO . Text.putStrLn . convertText . show

    delete :: InstanceSpec.Name -> m ExitCode
    delete = maybe success (exitCode <=< perform . OpDelete) <=< getStackId

    list :: m ExitCode
    list = do
      runConduit $ stackNames .| Conduit.mapM_ say
      success

    listTemplates :: m ExitCode
    listTemplates = do
      Foldable.mapM_
        (liftIO . Text.putStrLn . toText . Template.name)
        =<< templateProvider
      success

    listSpecs :: m ExitCode
    listSpecs = do
      Foldable.mapM_
        (liftIO . Text.putStrLn . toText . InstanceSpec.name)
        =<< instanceSpecProvider
      success

    events :: InstanceSpec.Name -> m ExitCode
    events name = do
      runConduit $ listResource req CF.dsersStackEvents .| Conduit.mapM_ printEvent
      success
      where
        req = CF.describeStackEvents & CF.dseStackName .~ pure (toText name)

    watch :: InstanceSpec.Name -> m ExitCode
    watch name = do
      stackId <- getExistingStackId name
      void $ pollEvents (defaultPoll stackId) printEvent
      success

    waitForOperation :: Token -> Id -> m ExitCode
    waitForOperation token stackId =
      exitCode =<< waitForAccept RemoteOperation{..} printEvent

    printNewToken :: m ExitCode
    printNewToken = do
      say =<< newToken
      success

    render :: Template.Name -> m ExitCode
    render name = do
      template <- Template.get templateProvider name
      say . Text.decodeUtf8 . LBS.toStrict $ Template.encode template
      success

    success :: m ExitCode
    success = pure ExitSuccess

    exitCode = \case
      RemoteOperationSuccess -> success
      RemoteOperationFailure -> pure $ ExitFailure 1

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
      value <- ParameterValue . convertText <$> Text.many1 Text.anyChar
      void Text.endOfInput

      pure $ Parameter name value

    allowChar = \case
      '-'  -> True
      char -> Char.isDigit char || Char.isAlpha char

stackName :: Parser InstanceSpec.Name
stackName = InstanceSpec.Name <$> argument str (metavar "STACK")

templateName :: Parser Template.Name
templateName = Template.Name <$> argument str (metavar "TEMPLATE")

parameters :: Parser Parameters
parameters = Parameters.fromList <$> many parameter
