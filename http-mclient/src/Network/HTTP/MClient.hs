module Network.HTTP.MClient
  ( Env
  , ResponseDecoder
  , ResponseError(..)
  , Result
  , SendRequest
  , Transaction(..)
  , addContentType
  , addHeader
  , decodeContentType
  , decodeContentTypes
  , decodeJSON
  , decodeJSONBody
  , decodeJSONOk
  , decodeJSONStatus
  , decodeStatus
  , decodeStatuses
  , defaultTransaction
  , send
  , send'
  , sendRequest
  , setJSONBody
  )
where

import Control.Arrow (left)
import Control.Monad.Reader (asks)
import Data.Conversions (Conversion(..), toText)
import MIO.Core
import MPrelude

import qualified Control.Retry              as Retry
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List                  as List
import qualified MIO.Log                    as Log
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Types         as HTTP
import qualified UnliftIO.Exception         as Exception

type SendRequest = HTTP.Request -> IO (HTTP.Response LBS.ByteString)

type Env env = (HasField "httpSendRequest" env SendRequest, Log.Env env)

data ResponseError
  = BodyDecodeFailure Text
  | HTTPError HTTP.HttpExceptionContent
  | UnexpectedContentType BS.ByteString
  | MissingContentType
  | UnexpectedStatusCode HTTP.Status
  deriving stock Show

instance Exception.Exception ResponseError

type Result a = Either ResponseError a

type ResponseDecoder a = HTTP.Response LBS.ByteString -> Result a

data Transaction a = Transaction
  { decoder     :: HTTP.HttpExceptionContent -> Result a
  , shouldRetry :: ResponseError             -> Bool
  , retryPolicy :: Retry.RetryPolicy
  }

decodeStatus
  :: HTTP.Status
  -> ResponseDecoder a
  -> ResponseDecoder a
decodeStatus expectedStatus decoder = decodeStatuses [(expectedStatus, decoder)]

decodeStatuses
  :: [(HTTP.Status, ResponseDecoder a)]
  -> ResponseDecoder a
decodeStatuses decoders response
  = maybe (Left $ UnexpectedStatusCode statusCode) ($ response)
  $ List.lookup statusCode decoders
  where
    statusCode = HTTP.responseStatus response

decodeContentType :: BS.ByteString -> ResponseDecoder a -> ResponseDecoder a
decodeContentType accepted decoder = decodeContentTypes [(accepted, decoder)]

decodeContentTypes :: [(BS.ByteString, ResponseDecoder a)] -> ResponseDecoder a
decodeContentTypes contentTypes response
  = maybe (Left MissingContentType) withResponseContentType
  $ List.lookup HTTP.hContentType (HTTP.responseHeaders response)
  where
    withResponseContentType contentType =
      maybe
        (Left $ UnexpectedContentType contentType)
        ($ response)
        (List.lookup contentType contentTypes)

decodeJSON  :: JSON.FromJSON a => ResponseDecoder a
decodeJSON = decodeContentTypes
 [ (jsonContentType,       decodeJSONBody)
 , (jsonLegacyContentType, decodeJSONBody)
 ]

decodeJSONBody :: JSON.FromJSON a => ResponseDecoder a
decodeJSONBody = left (BodyDecodeFailure . toText) . JSON.eitherDecode' . HTTP.responseBody

decodeJSONStatus :: JSON.FromJSON a => HTTP.Status -> ResponseDecoder a
decodeJSONStatus expectedStatus = decodeStatus expectedStatus decodeJSON

decodeJSONOk :: JSON.FromJSON a => ResponseDecoder a
decodeJSONOk = decodeJSONStatus HTTP.status200

setJSONBody :: JSON.ToJSON a => a -> HTTP.Request -> HTTP.Request
setJSONBody value request
  = addContentType jsonContentType
  $ request
  { HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode value
  }

addContentType :: BS.ByteString -> HTTP.Request -> HTTP.Request
addContentType = addHeader HTTP.hContentType

addHeader :: HTTP.HeaderName -> BS.ByteString -> HTTP.Request -> HTTP.Request
addHeader headerName value request =
  request { HTTP.requestHeaders = (headerName,value):HTTP.requestHeaders request }

jsonContentType :: BS.ByteString
jsonContentType = "application/json; charset=utf-8"

jsonLegacyContentType :: BS.ByteString
jsonLegacyContentType = "application/json"

defaultTransaction :: Transaction a
defaultTransaction = Transaction
  { decoder     = Left . HTTPError
  , retryPolicy = Retry.constantDelay 50000 <> Retry.limitRetries 1
  , ..
  }
  where
    shouldRetry :: ResponseError -> Bool
    shouldRetry = \case
      HTTPError (HTTP.ConnectionFailure _) -> True
      _                                    -> False

send
  :: Env env
  => ResponseDecoder a
  -> HTTP.Request
  -> MIO env (Result a)
send = send' defaultTransaction

send'
  :: Env env
  => Transaction a
  -> ResponseDecoder a
  -> HTTP.Request
  -> MIO env (Result a)
send' transaction decoder request = do
  sendRequest' <- asks (.httpSendRequest)
  sendRequest sendRequest' transaction decoder request

sendRequest
  :: forall env a . Env env
  => SendRequest
  -> Transaction a
  -> ResponseDecoder a
  -> HTTP.Request
  -> MIO env (Result a)
sendRequest sendRequest' Transaction{ decoder = transactionDecoder, .. } decoder request
  = Retry.retrying
    retryPolicy
    isRetriable
  $ const retriableSendRequest
  where
    retriableSendRequest :: MIO env (Result a)
    retriableSendRequest
      = either transactionDecoder decoder
      <$> Exception.tryJust selectException (liftIO $ sendRequest' request)

    isRetriable
      :: Retry.RetryStatus
      -> Result a
      -> MIO env Bool
    isRetriable Retry.RetryStatus{..}
      = either
        process
        (const (pure False))
      where
        process :: ResponseError -> MIO env Bool
        process error = if shouldRetry error then logWarning error $> True else pure False

        logWarning :: ResponseError -> MIO env ()
        logWarning error
          = Log.warn
          $ "Retrying failed request due to "
          <> showc error
          <> " attempt "
          <> showc rsIterNumber

    selectException :: HTTP.HttpException -> Maybe HTTP.HttpExceptionContent
    selectException = \case
      (HTTP.HttpExceptionRequest _request content) -> pure content
      _other                                       -> empty

showc :: forall a b . (Show a, Conversion b String) => a -> b
showc = convert . show
