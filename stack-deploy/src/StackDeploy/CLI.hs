module StackDeploy.CLI (exitCode, parserInfo) where

import CLI.Utils
import Control.Applicative (many)
import Control.Lens ((.~))
import Data.Conduit ((.|), runConduit)
import StackDeploy.CLI.Utils
import StackDeploy.Events
import StackDeploy.IO
import StackDeploy.Parameters
import StackDeploy.Prelude
import StackDeploy.Types
import StackDeploy.Wait

import qualified Amazonka.CloudFormation.CancelUpdateStack   as CF
import qualified Amazonka.CloudFormation.DescribeStackEvents as CF
import qualified Amazonka.CloudFormation.Types               as CF
import qualified Data.Attoparsec.Text                        as Text
import qualified Data.ByteString.Lazy                        as LBS
import qualified Data.Char                                   as Char
import qualified Data.Conduit.Combinators                    as Conduit
import qualified Data.Map.Strict                             as Map
import qualified Data.Text.Encoding                          as Text
import qualified Data.Text.IO                                as Text
import qualified MIO.Amazonka                                as AWS
import qualified Options.Applicative                         as CLI
import qualified StackDeploy.AWS                             as AWS
import qualified StackDeploy.Env                             as StackDeploy
import qualified StackDeploy.InstanceSpec                    as StackDeploy
import qualified StackDeploy.NamedTemplate                   as StackDeploy
import qualified StackDeploy.Operation                       as StackDeploy
import qualified StackDeploy.Stack                           as StackDeploy
import qualified Stratosphere                                as CFT
import qualified System.Exit                                 as System

parserInfo
  :: forall env . (AWS.Env env, StackDeploy.Env env)
  => StackDeploy.InstanceSpecMap env
  -> CLI.ParserInfo (MIO env System.ExitCode)
parserInfo instanceSpecMap = wrapHelper commands "stack commands"
  where
    commands :: CLI.Parser (MIO env System.ExitCode)
    commands = CLI.hsubparser
      $  mkCommand "instance" instanceCommands     "instance commands"
      <> mkCommand "spec"     specCommands         "instance spec commands"
      <> mkCommand "token"    (pure printNewToken) "print a new stack token"
      <> mkCommand "template" templateCommands     "template commands"

    instanceCommands :: CLI.Parser (MIO env System.ExitCode)
    instanceCommands = CLI.hsubparser
      $  mkCommand "cancel"     (cancel         <$> instanceNameOption)                 "cancel stack update"
      <> mkCommand "create"     (create         <$> instanceNameOption <*> parameters)  "create stack"
      <> mkCommand "delete"     (delete         <$> instanceNameOption)                 "delete stack"
      <> mkCommand "events"     (events         <$> instanceNameOption)                 "list stack events"
      <> mkCommand "outputs"    (outputs        <$> instanceNameOption)                 "list stack outputs"
      <> mkCommand "parameters" (listParameters <$> instanceNameOption)                 "list stack parameters"
      <> mkCommand "sync"       (sync           <$> instanceNameOption <*> parameters)  "sync stack with spec"
      <> mkCommand "update"     (update         <$> instanceNameOption <*> parameters)  "update existing stack"
      <> mkCommand "wait"       (wait           <$> instanceNameOption <*> tokenOption) "wait for stack operation"
      <> mkCommand "watch"      (watch          <$> instanceNameOption)                 "watch stack events"

    templateCommands :: CLI.Parser (MIO env System.ExitCode)
    templateCommands = CLI.hsubparser
      $  mkCommand "list"    (pure listTemplates)            "list templates"
      <> mkCommand "render"  (render <$> templateNameOption) "render template"

    specCommands :: CLI.Parser (MIO env System.ExitCode)
    specCommands = CLI.hsubparser
      $  mkCommand "list"   (pure listSpecs) "list stack specifications"

    cancel :: StackDeploy.InstanceName -> MIO env System.ExitCode
    cancel instanceName = do
      void . AWS.send . CF.newCancelUpdateStack $ toText instanceName
      success

    create :: StackDeploy.InstanceName -> ParameterMap -> MIO env System.ExitCode
    create instanceName userParameterMap = do
      withInstanceSpec instanceName $ \instanceSpec ->
        exitCode =<< StackDeploy.performOperation (OpCreate instanceSpec userParameterMap)

    update :: StackDeploy.InstanceName -> ParameterMap -> MIO env System.ExitCode
    update instanceName userParameterMap = do
      withInstanceSpec instanceName $ \instanceSpec ->
        withExistingStack instanceName $ \existingStack ->
          exitCode =<< StackDeploy.performOperation (OpUpdate existingStack instanceSpec userParameterMap)

    sync :: StackDeploy.InstanceName -> ParameterMap -> MIO env System.ExitCode
    sync instanceName userParameterMap = do
      withInstanceSpec instanceName $ \instanceSpec -> do
        exitCode
          =<< StackDeploy.performOperation . maybe
                (OpCreate instanceSpec userParameterMap)
                (\existingStack -> OpUpdate existingStack instanceSpec userParameterMap)
          =<< StackDeploy.readExistingStack instanceName

    wait :: StackDeploy.InstanceName -> Token -> MIO env System.ExitCode
    wait instanceName token
      = withExistingStack instanceName
      $ waitForOperation token . (.stackId)

    outputs :: StackDeploy.InstanceName -> MIO env System.ExitCode
    outputs instanceName = withExistingStack instanceName $ \existingStack -> do
      traverse_ printOutput existingStack.outputs
      success
      where
        printOutput :: CF.Output -> MIO env ()
        printOutput = liftIO . Text.putStrLn . convertText . show

    listParameters :: StackDeploy.InstanceName -> MIO env System.ExitCode
    listParameters instanceName = withExistingStack instanceName $ \existingStack -> do
      traverse_ printParameter existingStack.parameters
      success
      where
        printParameter :: CF.Parameter -> MIO env ()
        printParameter parameter
          = liftIO
          . Text.putStrLn
          $ fromMaybe "" parameter.parameterKey <> ": " <> fromMaybe "" (convert parameter.parameterValue)

    delete :: StackDeploy.InstanceName -> MIO env System.ExitCode
    delete instanceName = withExistingStack instanceName $ exitCode <=< StackDeploy.performOperation . OpDelete

    listTemplates :: MIO env System.ExitCode
    listTemplates = do
      printList $ Map.keys namedTemplateMap
      success

    listSpecs :: MIO env System.ExitCode
    listSpecs = do
      printList $ Map.keys instanceSpecMap
      success

    events :: StackDeploy.InstanceName -> MIO env System.ExitCode
    events instanceName = do
      runConduit $ AWS.nestedResourceC req (fromMaybe [] . (.stackEvents)) .| Conduit.mapM_ StackDeploy.printEvent
      success
      where
        req = CF.newDescribeStackEvents & CF.describeStackEvents_stackName .~ pure (toText instanceName)

    watch :: StackDeploy.InstanceName -> MIO env System.ExitCode
    watch instanceName = withExistingStack instanceName $ \existingStack -> do
      void $ pollEvents (defaultPoll existingStack.stackId) StackDeploy.printEvent
      success

    waitForOperation :: Token -> StackId -> MIO env System.ExitCode
    waitForOperation token stackId =
      exitCode =<< waitForAccept RemoteOperation{..} StackDeploy.printEvent

    printNewToken :: MIO env System.ExitCode
    printNewToken = do
      say =<< newToken
      success

    render :: StackDeploy.TemplateName -> MIO env System.ExitCode
    render templateName =
      maybe
        (failure $ "Template not found: " <> convert templateName)
        printPretty
        (Map.lookup templateName namedTemplateMap)
      where
        printPretty :: CFT.Template -> MIO env System.ExitCode
        printPretty template = do
          say
            . Text.decodeUtf8
            . LBS.toStrict
            $ StackDeploy.stratosphereTemplateEncodePretty template
          pure System.ExitSuccess

    namedTemplateMap = StackDeploy.templateMapFromInstanceSpecMap instanceSpecMap

    withExistingStack
      :: StackDeploy.InstanceName
      -> (ExistingStack -> MIO env System.ExitCode)
      -> MIO env System.ExitCode
    withExistingStack instanceName action =
      StackDeploy.readExistingStack instanceName >>=
        maybe
          (failure $ "Stack does not exist: " <> convert instanceName)
          action

    withInstanceSpec
      :: StackDeploy.InstanceName
      -> (StackDeploy.InstanceSpec env -> MIO env System.ExitCode)
      -> MIO env System.ExitCode
    withInstanceSpec instanceName action =
      maybe
        (failure $ "Instance spec does not exist: " <> convert instanceName)
        action
        (Map.lookup instanceName instanceSpecMap)

    printList :: Conversion Text a => [a] -> MIO env ()
    printList = traverse_ say

parameters :: CLI.Parser ParameterMap
parameters = fromList . fmap (\Parameter{..} -> (name, value)) <$> many parameter
  where
    parameter :: CLI.Parser Parameter
    parameter = CLI.option
      reader
      (CLI.long "parameter" <> CLI.help "Set stack parameter")
      where
        reader = CLI.eitherReader (Text.parseOnly parser . convertText)

        parser = do
          instanceName <- convertFail =<< Text.many1 (Text.satisfy allowChar)
          Text.skip (== ':')
          value <- convertFail =<< Text.many' Text.anyChar
          void Text.endOfInput

          pure $ Parameter instanceName value

        allowChar = \case
          '-'  -> True
          char -> Char.isDigit char || Char.isAlpha char
