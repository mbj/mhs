module Network.HTTP.MClient.Decoder
  ( Env
  , ResponseDecoder
  , ResponseError
  , SendRequest
  , decodeJSON
  , decodeStatus
  , decodeStatuses
  , send
  )
where

import Control.Arrow (left)
import Data.Conversions (toText)
import GHC.Records (HasField(..))
import MPrelude

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List                  as List
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Types         as HTTP
import qualified UnliftIO.Exception         as Exception

type SendRequest = HTTP.Request -> IO (HTTP.Response LBS.ByteString)

type Env env = HasField "httpSendRequest" env SendRequest

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

decodeExpectedContentType :: BS.ByteString -> ResponseDecoder a -> ResponseDecoder a
decodeExpectedContentType accepted decoder response =
  maybe (Left MissingContentType) withResponseContentType parseResponseContentType
  where
    parseResponseContentType :: Maybe BS.ByteString
    parseResponseContentType =
      List.lookup HTTP.hContentType (HTTP.responseHeaders response)

    withResponseContentType responseContentType =
      if responseContentType == accepted
        then decoder response
        else Left $ UnexpectedContentType responseContentType

decodeJSON  :: JSON.FromJSON a => ResponseDecoder a
decodeJSON = decodeExpectedContentType "application/json; charset=utf-8" $ \response ->
  left (BodyDecodeFailure . toText) $ JSON.eitherDecode' (HTTP.responseBody response)

send
  :: (HTTP.Request -> IO (HTTP.Response LBS.ByteString))
  -> ResponseDecoder a
  -> HTTP.Request
  -> IO (Result a)
send sendRequest decoder request =
  either (Left . HTTPError) decoder <$> Exception.tryJust selectException (sendRequest request)
  where
    selectException :: HTTP.HttpException -> Maybe HTTP.HttpExceptionContent
    selectException = \case
      (HTTP.HttpExceptionRequest _request content) -> pure content
      _other                                       -> empty
