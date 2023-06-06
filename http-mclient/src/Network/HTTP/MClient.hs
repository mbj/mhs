module Network.HTTP.MClient
  ( Env
  , ResponseDecoder
  , ResponseError(..)
  , SendRequest
  , TransactionDecoder
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
  , send
  , send'
  , sendRequest
  , setJSONBody
  )
where

import Control.Arrow (left)
import Control.Monad.Reader (asks)
import Data.Conversions (Conversion(..), convertImpure, toText)
import GHC.Records (HasField(..))
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

type TransactionDecoder a = HTTP.HttpExceptionContent -> Result a

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

send
  :: Env env
  => ResponseDecoder a
  -> HTTP.Request
  -> MIO env (Result a)
send = send' (Left . HTTPError)

send'
  :: Env env
  => TransactionDecoder a
  -> ResponseDecoder a
  -> HTTP.Request
  -> MIO env (Result a)
send' transactionDecoder decoder request = do
  sendRequest' <- asks (.httpSendRequest)
  sendRequest sendRequest' transactionDecoder decoder request

sendRequest
  :: forall env a . Env env
  => SendRequest
  -> TransactionDecoder a
  -> ResponseDecoder a
  -> HTTP.Request
  -> MIO env (Result a)
sendRequest sendRequest' transactionDecoder decoder request =
  either transactionDecoder decoder <$> retriableSendRequest
  where
    retriableSendRequest :: MIO env (Either HTTP.HttpExceptionContent (HTTP.Response LBS.ByteString))
    retriableSendRequest
      = Retry.retrying
        singleRetry
        isRetriable
      $ const (Exception.tryJust selectException . liftIO $ sendRequest' request)
      where
        singleRetry :: Retry.RetryPolicy
        singleRetry = Retry.constantDelay 50_000 <> Retry.limitRetries (convertImpure retryLimit)

        isRetriable
          :: Retry.RetryStatus
          -> Either HTTP.HttpExceptionContent (HTTP.Response LBS.ByteString)
          -> MIO env Bool
        isRetriable Retry.RetryStatus{..} = either process (const (pure False))
          where
            process :: HTTP.HttpExceptionContent -> MIO env Bool
            process = \case
              error@(HTTP.ConnectionFailure _) -> logWarning error $> True
              _                                -> pure False
              where
                logWarning error
                  = Log.warn
                  $ "Retrying failed request due to "
                  <> showc error
                  <> " attempt "
                  <> showc rsIterNumber
                  <> " of "
                  <> showc retryLimit

        retryLimit :: Natural
        retryLimit = 1

        selectException :: HTTP.HttpException -> Maybe HTTP.HttpExceptionContent
        selectException = \case
          (HTTP.HttpExceptionRequest _request content) -> pure content
          _other                                       -> empty

showc :: forall a b . (Show a, Conversion b String) => a -> b
showc = convert . show
