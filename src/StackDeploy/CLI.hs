module StackDeploy.CLI (instanceSpecName, parserInfo) where

import Control.Applicative (many)
import Control.Lens ((&), (.~), view)
import Control.Monad ((<=<), mapM_)
import Data.Conduit ((.|), runConduit)
import Data.String (String)
import Options.Applicative hiding (value)
import StackDeploy.AWS
import StackDeploy.Events
import StackDeploy.IO
import StackDeploy.Parameters (Parameters)
import StackDeploy.Prelude
import StackDeploy.Stack
import StackDeploy.Types
import StackDeploy.Wait
import System.Exit (ExitCode(..))

import qualified Data.Attoparsec.Text                           as Text
import qualified Data.ByteString.Lazy                           as LBS
import qualified Data.Char                                      as Char
import qualified Data.Conduit.Combinators                       as Conduit
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
parserInfo templateProvider instanceSpecProvider = wrapHelper commands
  where
    commands :: Parser (m ExitCode)
    commands = hsubparser
      $  mkCommand "cancel"  (cancel <$> instanceSpecName)
      <> mkCommand "create"  (create <$> instanceSpecName <*> parameters)
      <> mkCommand "delete"  (delete <$> instanceSpecName)
      <> mkCommand "events"  (events <$> instanceSpecName)
      <> mkCommand "list"    (pure list)
      <> mkCommand "outputs" (outputs <$> instanceSpecName)
      <> mkCommand "render"  (render <$> templateName)
      <> mkCommand "sync"    (sync <$> instanceSpecName <*> parameters)
      <> mkCommand "token"   (pure printNewToken)
      <> mkCommand "update"  (update <$> instanceSpecName <*> parameters)
      <> mkCommand "wait"    (wait <$> instanceSpecName <*> tokenParser)
      <> mkCommand "watch"   (watch <$> instanceSpecName)

    tokenParser :: Parser Token
    tokenParser = Token <$> argument str (metavar "TOKEN")

    mkCommand :: String -> Parser b -> Mod CommandFields b
    mkCommand name = command name . wrapHelper

    wrapHelper :: Parser b -> ParserInfo b
    wrapHelper parser = info (helper <*> parser) idm

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

parameter :: Parser (Text, CF.Parameter)
parameter = option
  parameterReader
  (long "parameter" <> help "Set stack parameter")

parameterReader :: ReadM (Text, CF.Parameter)
parameterReader = eitherReader (Text.parseOnly parser . convertText)
  where
    parser = do
      key <- convertText <$> Text.many1 (Text.satisfy allowChar)
      Text.skip (== ':')
      value <- convertText <$> Text.many1 Text.anyChar
      void Text.endOfInput

      pure (key, mkCFParam key value)

    allowChar = \case
      '-'  -> True
      char -> Char.isDigit char || Char.isAlpha char

    mkCFParam key value
      = CF.parameter
      & CF.pParameterKey .~ pure key
      & CF.pParameterValue .~ pure value


instanceSpecName :: Parser InstanceSpec.Name
instanceSpecName = InstanceSpec.Name <$> argument str (metavar "INSTANCE_NAME")

templateName :: Parser Template.Name
templateName = Template.Name <$> argument str (metavar "TEMPLATE_NAME")

parameters :: Parser Parameters
parameters = Parameters.fromList <$> many parameter
