module AWS.Lambda.Runtime.Client
  ( Connection
  , InternalLambdaClientError(..)
  , LambdaClient
  , getConnection
  , getNextEvent
  , sendBootError
  , sendEventResponse
  )
where

import AWS.Lambda.Runtime.Prelude
import AWS.Lambda.Runtime.Types
import Control.Exception (displayException)
import Control.Monad.Except (ExceptT(..), liftEither, throwError, withExceptT)
import Data.List (filter, map)
import Data.Tuple (fst, snd)
import System.Environment (lookupEnv)

import qualified Data.Aeson          as JSON
import qualified Data.ByteString     as BS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types  as HTTP
import qualified XRay.TraceHeader    as XRay

data Connection = Connection
  { request :: HTTP.Request
  , manager :: HTTP.Manager
  }

data InternalLambdaClientError
  = ConnectionError HTTP.HttpException
  | InvalidLambdaRunTimeApi Text
  | InvalidTraceId Text
  | LambdaContextDecodeError Text
  | MissingLambdaRunTimeApi
  | PayloadTooLarge
  | ResponseBodyDecodeFailure Text
  | UnSuccessfulEventRetrieval HTTP.Status Text
  | UnSuccessfulEventSending HTTP.Status Text
  deriving stock Show

instance Exception InternalLambdaClientError

type LambdaClient = ExceptT InternalLambdaClientError IO

getConnection :: LambdaClient Connection
getConnection = do
  awsLambdaRuntimeApi <- ExceptT $ maybeToEither MissingLambdaRunTimeApi
    <$> lookupEnv "AWS_LAMBDA_RUNTIME_API"

  request <- withExceptT (InvalidLambdaRunTimeApi . showc @Text)
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
  pure $ Connection{..}

getNextEvent:: JSON.FromJSON a => Connection -> LambdaClient (Event a)
getNextEvent connection = do
  response <- getNextEventValue connection

  requestId   <- RequestId <$> liftEither (fetchHeader response "Lambda-Runtime-Aws-Request-Id")
  traceHeader <- parseTraceHeader =<< liftEither (fetchHeader response "Lambda-Runtime-Trace-Id")

  pure Event
    { body = HTTP.responseBody response
    , ..
    }
  where
    fetchHeader
      :: HTTP.Response a
      -> HTTP.HeaderName
      -> Either InternalLambdaClientError Text
    fetchHeader response header
      = maybeToEither (LambdaContextDecodeError . convert $ "Missing header: " <> show header)
      $ getResponseHeader header response

parseTraceHeader :: Text -> LambdaClient XRay.TraceHeader
parseTraceHeader value
  = liftEither
  . first (const $ InvalidTraceId value)
  $ XRay.parseTraceHeader value

getNextEventValue :: JSON.FromJSON a => Connection -> LambdaClient (HTTP.Response a)
getNextEventValue Connection{..} = do
  response <- performHttpRequest $ Connection { request = toNextEventRequest request, .. }

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
  :: (JSON.ToJSON a, MonadCatch m, MonadIO m)
  => Connection
  -> RequestId
  -> a
  -> m ()
sendEventResponse Connection{..} requestId value = do
  response <- catchConnectionError
    . flip HTTP.httpNoBody manager
    $ toEventSuccessRequest request

  let statusCode = HTTP.responseStatus response

  when (statusCode == HTTP.status413) $ throwM PayloadTooLarge

  unless (HTTP.statusIsSuccessful statusCode) .
    throwM $ UnSuccessfulEventSending statusCode "Could not post handler result"
  where
    toEventSuccessRequest :: HTTP.Request -> HTTP.Request
    toEventSuccessRequest request' = request'
      { HTTP.requestBody = HTTP.RequestBodyLBS (JSON.encode value)
      , HTTP.method      = "POST"
      , HTTP.path        = "2018-06-01/runtime/invocation/" <> convert requestId <> "/response"
      }

sendBootError :: MonadIO m => Connection -> InternalLambdaClientError -> m ()
sendBootError Connection{..} error
  = void
  . liftIO
  . flip HTTP.httpNoBody manager
  $ toInitErrorRequest request
  where
    toInitErrorRequest :: HTTP.Request -> HTTP.Request
    toInitErrorRequest request' = (toBaseErrorRequest error request')
      { HTTP.path = "2018-06-01/runtime/init/error"
      }

performHttpRequest :: JSON.FromJSON a => Connection -> LambdaClient (HTTP.Response a)
performHttpRequest Connection{..} = do
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
