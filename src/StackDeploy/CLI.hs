module StackDeploy.CLI (parserInfo) where

import Control.Applicative (many)
import Control.Exception.Base (AssertionFailed(AssertionFailed))
import Control.Lens ((&), (.~))
import Control.Monad ((<=<))
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.AWS (AWSConstraint)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isAlpha, isDigit)
import Data.Conduit ((.|), runConduit)
import Data.Conduit.Combinators (mapM_)
import Data.String (String)
import Data.Text.Encoding (decodeUtf8)
import Network.AWS (MonadAWS)
import Network.AWS.CloudFormation.DescribeStackEvents
  ( describeStackEvents
  , dseStackName
  , dsersStackEvents
  )
import Network.AWS.CloudFormation.Types
  ( Parameter
  , pParameterKey
  , pParameterValue
  , parameter
  )
import Options.Applicative
  ( CommandFields
  , Mod
  , Parser
  , ParserInfo
  , ReadM
  , argument
  , command
  , eitherReader
  , help
  , helper
  , hsubparser
  , idm
  , info
  , long
  , metavar
  , option
  , str
  )
import StackDeploy.AWS
import StackDeploy.Events
import StackDeploy.IO
import StackDeploy.Prelude
import StackDeploy.Stack
import StackDeploy.Types
import StackDeploy.Wait
import Stratosphere (Template, encodeTemplate)
import System.Exit (ExitCode(..))

import qualified Data.Attoparsec.Text as Text

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
      $  mkCommand "create" (create <$> nameParser <*> many parameterParser)
      <> mkCommand "delete" (delete <$> nameParser)
      <> mkCommand "events" (events <$> nameParser)
      <> mkCommand "list"   (pure list)
      <> mkCommand "render" (render <$> nameParser)
      <> mkCommand "sync"   (sync <$> nameParser <*> many parameterParser)
      <> mkCommand "token"  (pure printNewToken)
      <> mkCommand "update" (update <$> nameParser <*> many parameterParser)
      <> mkCommand "wait"   (wait <$> nameParser <*> tokenParser)
      <> mkCommand "watch"  (watch <$> nameParser)

    nameParser :: Parser Name
    nameParser = Name <$> argument str (metavar "NAME")

    tokenParser :: Parser Token
    tokenParser = Token <$> argument str (metavar "TOKEN")

    mkCommand :: String -> Parser b -> Mod CommandFields b
    mkCommand name = command name . wrapHelper

    wrapHelper :: Parser b -> ParserInfo b
    wrapHelper parser = info (helper <*> parser) idm

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

    delete :: Name -> m ExitCode
    delete = maybe success (exitCode <=< perform . OpDelete) <=< getStackId

    list :: m ExitCode
    list = do
      runConduit $ stackNames .| mapM_ say
      success

    events :: Name -> m ExitCode
    events name = do
      runConduit $ listResource req dsersStackEvents .| mapM_ printEvent
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
      void $ Text.char ':'
      value <- convertText <$> Text.many1 (Text.satisfy allowChar)

      pure
        $ parameter
        & pParameterKey .~ pure key
        & pParameterValue .~ pure value

    allowChar char = isDigit char || isAlpha char

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
