module AWS.Lambda.Client
  ( HTTPConfig
  , InternalLambdaClientError (..)
  , LambdaClient
  , LambdaEvent (..)
  , getHttpConfig
  , getNextLambdaEvent
  , sendBootError
  , sendEventResponse
  )
where

import AWS.Prelude
import Control.Exception (displayException)
import Control.Monad.Except (ExceptT(..), liftEither, throwError, withExceptT)
import System.Environment (lookupEnv)

import qualified Data.Aeson          as JSON
import qualified Data.ByteString     as BS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types  as HTTP

data HTTPConfig = HTTPConfig
  { request :: HTTP.Request
  , manager :: HTTP.Manager
  }

data LambdaEvent = LambdaEvent
  { event     :: JSON.Value
  , requestId :: RequestId
  }
  deriving stock Show

newtype RequestId = RequestId Text
  deriving stock (Show)

instance Conversion BS.ByteString RequestId where
  convert (RequestId requestId) = encodeUtf8 requestId

data InternalLambdaClientError
  = ConnectionError HTTP.HttpException
  | InvalidLambdaRunTimeApi Text
  | LambdaContextDecodeError Text
  | MissingLambdaRunTimeApi
  | PayloadTooLarge Text
  | ResponseBodyDecodeFailure Text
  | UnSuccessfulEventRetrieval HTTP.Status Text
  | UnSuccessfulEventSending HTTP.Status Text
  deriving stock Show

instance Exception InternalLambdaClientError

type LambdaClient = ExceptT InternalLambdaClientError IO

getHttpConfig :: LambdaClient HTTPConfig
getHttpConfig = do
  awsLambdaRuntimeApi <- ExceptT $ maybeToEither MissingLambdaRunTimeApi
    <$> lookupEnv "AWS_LAMBDA_RUNTIME_API"

  req <- withExceptT (InvalidLambdaRunTimeApi . showc @Text)
    . ExceptT
    . try @_ @HTTP.HttpException
    $ HTTP.parseRequest ("http://" <> awsLambdaRuntimeApi)

  manager <- liftIO $ HTTP.newManager
    $ HTTP.managerSetProxy HTTP.noProxy
    $ HTTP.defaultManagerSettings
        { HTTP.managerResponseTimeout     = HTTP.responseTimeoutNone
        , HTTP.managerConnCount           = 1
        , HTTP.managerIdleConnectionCount = 1
        }
  pure $ HTTPConfig req manager

getNextLambdaEvent:: HTTPConfig -> LambdaClient LambdaEvent
getNextLambdaEvent httpConfig = do
  nextEvent <- getNextLambdaEventValue httpConfig
  requestId <- RequestId <$> liftEither (getRequestId nextEvent)

  pure LambdaEvent
    { event = HTTP.responseBody nextEvent
    , ..
    }
  where
    getRequestId
      :: HTTP.Response JSON.Value
      -> Either InternalLambdaClientError Text
    getRequestId response
      = maybeToEither (LambdaContextDecodeError "Missing request Id")
      $ getResponseHeader "Lambda-Runtime-Aws-Request-Id" response

getNextLambdaEventValue :: HTTPConfig -> LambdaClient (HTTP.Response JSON.Value)
getNextLambdaEventValue HTTPConfig{..} = do
  response <- performHttpRequest $ HTTPConfig { request = toNextEventRequest request, .. }

  let statusCode = HTTP.responseStatus response

  unless (HTTP.statusIsSuccessful statusCode) . throwError
    $ UnSuccessfulEventRetrieval statusCode "Could not retrieve next event"

  pure response
  where
    toNextEventRequest :: HTTP.Request -> HTTP.Request
    toNextEventRequest req = req
      { HTTP.path = "2018-06-01/runtime/invocation/next"
      }

sendEventResponse
  :: (MonadCatch m, MonadIO m)
  => HTTPConfig
  -> RequestId
  -> JSON.Value
  -> m ()
sendEventResponse HTTPConfig{..} requestId json = do
  response <- catchConnectionError
    . flip HTTP.httpNoBody manager
    $ toEventSuccessRequest request

  let statusCode = HTTP.responseStatus response

  when (statusCode == HTTP.status413) . throwM . PayloadTooLarge $ showc json

  unless (HTTP.statusIsSuccessful statusCode) .
    throwM $ UnSuccessfulEventSending statusCode "Could not post handler result"
  where
    toEventSuccessRequest :: HTTP.Request -> HTTP.Request
    toEventSuccessRequest req = req
      { HTTP.requestBody = HTTP.RequestBodyLBS (JSON.encode json)
      , HTTP.method      = "POST"
      , HTTP.path        = "2018-06-01/runtime/invocation/" <> convert requestId <> "/response"
      }

sendBootError :: MonadIO m => HTTPConfig -> InternalLambdaClientError -> m ()
sendBootError HTTPConfig{..} error
  = void
  . liftIO
  . flip HTTP.httpNoBody manager
  $ toInitErrorRequest request
  where
    toInitErrorRequest :: HTTP.Request -> HTTP.Request
    toInitErrorRequest req = (toBaseErrorRequest error req)
      { HTTP.path = "2018-06-01/runtime/init/error"
      }

performHttpRequest :: HTTPConfig -> LambdaClient (HTTP.Response JSON.Value)
performHttpRequest HTTPConfig{..} = do
  response <- catchConnectionError $ HTTP.httpLbs request manager

  body <- liftEither
    . first (ResponseBodyDecodeFailure . convert)
    . JSON.eitherDecode
    $ HTTP.responseBody response

  pure $ fmap (const body) response

catchConnectionError :: (MonadCatch m, MonadIO m) => IO a -> m a
catchConnectionError action =
  catch (liftIO action)
    $ \e -> throwM . ConnectionError $ (e :: HTTP.HttpException)

toBaseErrorRequest :: Exception e => e -> HTTP.Request -> HTTP.Request
toBaseErrorRequest (displayException -> error) req = req
  { HTTP.requestBody    = HTTP.RequestBodyLBS (JSON.encode error)
  , HTTP.method         = "POST"
  , HTTP.requestHeaders = [("Content-Type", "application/vnd.aws.lambda.error+json")]
  }

getResponseHeader :: HTTP.HeaderName -> HTTP.Response a -> Maybe Text
getResponseHeader name = fmap decodeUtf8 . getRawResponseHeader name

getRawResponseHeader :: HTTP.HeaderName -> HTTP.Response a -> Maybe BS.ByteString
getRawResponseHeader name
  = safeHead
  . map snd
  . filter ((name ==) . fst)
  . HTTP.responseHeaders