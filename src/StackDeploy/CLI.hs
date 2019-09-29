module StackDeploy.CLI (nameParser, parserInfo) where

import Control.Applicative (many)
import Control.Exception.Base (AssertionFailed(AssertionFailed))
import Control.Lens ((&), (.~), view)
import Control.Monad ((<=<), mapM_)
import Control.Monad.Catch (throwM)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isAlpha, isDigit)
import Data.Conduit ((.|), runConduit)
import Data.String (String)
import Data.Text.Encoding (decodeUtf8)
import Network.AWS.CloudFormation.CancelUpdateStack
import Network.AWS.CloudFormation.DescribeStackEvents
import Network.AWS.CloudFormation.Types
import Options.Applicative hiding (value)
import StackDeploy.AWS
import StackDeploy.Events
import StackDeploy.IO
import StackDeploy.Prelude
import StackDeploy.Stack
import StackDeploy.Template
import StackDeploy.Types
import StackDeploy.Wait
import Stratosphere (Template)
import System.Exit (ExitCode(..))

import qualified Data.Attoparsec.Text     as Text
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Text.IO             as Text
import qualified Network.AWS              as AWS

type InstanceSpecProvider
  =  forall m r . (AWSConstraint r m, MonadAWS m)
  => (Name -> [Parameter] -> m InstanceSpec)

type TemplateProvider
  =  forall m . MonadIO m
  => (Name -> m Template)

parserInfo
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => TemplateProvider
  -> InstanceSpecProvider
  -> ParserInfo (m ExitCode)
parserInfo templateProvider instanceSpecProvider = wrapHelper commands
  where
    commands :: Parser (m ExitCode)
    commands = hsubparser
      $  mkCommand "cancel"  (cancel <$> nameParser)
      <> mkCommand "create"  (create <$> nameParser <*> many parameterParser)
      <> mkCommand "delete"  (delete <$> nameParser)
      <> mkCommand "events"  (events <$> nameParser)
      <> mkCommand "list"    (pure list)
      <> mkCommand "outputs" (outputs <$> nameParser)
      <> mkCommand "render"  (render <$> nameParser)
      <> mkCommand "sync"    (sync <$> nameParser <*> many parameterParser)
      <> mkCommand "token"   (pure printNewToken)
      <> mkCommand "update"  (update <$> nameParser <*> many parameterParser)
      <> mkCommand "wait"    (wait <$> nameParser <*> tokenParser)
      <> mkCommand "watch"   (watch <$> nameParser)

    tokenParser :: Parser Token
    tokenParser = Token <$> argument str (metavar "TOKEN")

    mkCommand :: String -> Parser b -> Mod CommandFields b
    mkCommand name = command name . wrapHelper

    wrapHelper :: Parser b -> ParserInfo b
    wrapHelper parser = info (helper <*> parser) idm

    cancel :: Name -> m ExitCode
    cancel name = do
      void . AWS.send . cancelUpdateStack $ toText name
      success

    create :: Name -> [Parameter] -> m ExitCode
    create name params = do
      spec     <- instanceSpecProvider name params
      template <- templateProvider name
      exitCode =<< perform (OpCreate name spec template)

    update :: Name -> [Parameter] -> m ExitCode
    update name params = do
      spec     <- instanceSpecProvider name params
      stackId  <- getExistingStackId name
      template <- templateProvider name

      exitCode =<< perform (OpUpdate stackId spec template)

    sync :: Name -> [Parameter] -> m ExitCode
    sync name params = do
      spec     <- instanceSpecProvider name params
      template <- templateProvider name

      exitCode
        =<< perform . maybe (OpCreate name spec template) (\stackId -> OpUpdate stackId spec template)
        =<< getStackId name

    wait :: Name -> Token -> m ExitCode
    wait name token = maybe success (waitForOperation token) =<< getStackId name

    outputs :: Name -> m ExitCode
    outputs name = do
      mapM_ printOutput =<< (view sOutputs <$> getExistingStack name)
      success
      where
        printOutput :: Output -> m ()
        printOutput = liftIO . Text.putStrLn . convertText . show

    delete :: Name -> m ExitCode
    delete = maybe success (exitCode <=< perform . OpDelete) <=< getStackId

    list :: m ExitCode
    list = do
      runConduit $ stackNames .| Conduit.mapM_ say
      success

    events :: Name -> m ExitCode
    events name = do
      runConduit $ listResource req dsersStackEvents .| Conduit.mapM_ printEvent
      success
      where
        req = describeStackEvents & dseStackName .~ pure (toText name)

    watch :: Name -> m ExitCode
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

    render :: Name -> m ExitCode
    render name = do
      template <- templateProvider name
      say . decodeUtf8 . toStrict $ encodeTemplate template
      success

    success :: m ExitCode
    success = pure ExitSuccess

    exitCode = \case
      RemoteOperationSuccess -> success
      RemoteOperationFailure -> pure $ ExitFailure 1

parameterParser :: Parser Parameter
parameterParser = option
  parameterReader
  (long "parameter" <> help "Set stack parameter")

parameterReader :: ReadM Parameter
parameterReader = eitherReader (Text.parseOnly parser . convertText)
  where
    parser = do
      key <- convertText <$> Text.many1 (Text.satisfy allowChar)
      Text.skip (== ':')
      value <- convertText <$> Text.many1 Text.anyChar
      void Text.endOfInput

      pure
        $ parameter
        & pParameterKey .~ pure key
        & pParameterValue .~ pure value

    allowChar = \case
      '-'  -> True
      char -> isDigit char || isAlpha char

getExistingStackId
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => Name
  -> m Id
getExistingStackId name = maybe throwNoStack pure =<< getStackId name
  where
    throwNoStack :: m a
    throwNoStack
      = throwM
      . AssertionFailed
      . convertText
      $ "No stack " <> toText name <> " found to update"

nameParser :: Parser Name
nameParser = Name <$> argument str (metavar "NAME")
