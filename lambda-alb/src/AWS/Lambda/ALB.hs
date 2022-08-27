module AWS.Lambda.ALB
  ( Header
  , HeaderName
  , Request(..)
  , Response(..)
  , ResponseBody
  , mkByteStringResponseBody
  , mkTextResponseBody
  , responseBodyByteString
  , responseBodyContentLength
  , run
  )
where

import AWS.Lambda.ALB.Header
import AWS.Lambda.Runtime.Prelude
import Data.Aeson ((.:))
import Data.Map.Strict (Map)

import qualified AWS.Lambda.Runtime     as Lambda.Runtime
import qualified Data.Aeson             as JSON
import qualified Data.Aeson.Key         as JSON.Key
import qualified Data.Aeson.KeyMap      as KeyMap
import qualified Data.Aeson.Types       as JSON
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.CaseInsensitive   as CI
import qualified Data.Text.Encoding     as Text
import qualified Network.HTTP.Types     as HTTP

newtype RequestDecodeFailure = RequestDecodeFailure Text
  deriving anyclass (Exception)
  deriving stock (Eq, Show)

data Request = Request
  { path                  :: Text
  , httpMethod            :: HTTP.StdMethod
  , headers               :: Headers
  , queryStringParameters :: Map Text Text
  , isBase64Encoded       :: Bool
  , requestContext        :: JSON.Value
  , body                  :: Text
  }
  deriving stock (Generic, Show, Eq)

instance JSON.FromJSON Request where
  parseJSON = JSON.withObject "ALB Request" $ \object -> do
    path                  <- object .: "path"
    httpMethod            <- parseMethod =<< object .: "httpMethod"
    headers               <- parseHeaders =<< object .: "headers"
    queryStringParameters <- object .: "queryStringParameters"
    isBase64Encoded       <- object .: "isBase64Encoded"
    requestContext        <- object .: "requestContext"
    body                  <- object .: "body"
    pure Request{..}
    where
      parseMethod = JSON.withText "HTTP Method"
        $ either (fail . show) pure
        . HTTP.parseMethod
        . Text.encodeUtf8

      parseHeaders = JSON.withObject "Headers" $ traverse toHeader . KeyMap.toList

      toHeader :: (JSON.Key, JSON.Value) -> JSON.Parser Header
      toHeader (headerName, value) =
        case value of
          JSON.String text -> pure (CI.mk $ JSON.Key.toText headerName, text)
          _                -> fail $ "Failure parsing header " <> JSON.Key.toString headerName <> " : " <> show value

data Response = Response
  { statusCode :: HTTP.Status
  , headers    :: Headers
  , body       :: ResponseBody
  }

instance JSON.ToJSON Response where
  toJSON Response{..}
    = JSON.Object
    [ ("body",            JSON.toJSON $ textBody body)
    , ("headers",         headersJSON)
    , ("isBase64Encoded", JSON.toJSON $ isByteString body)
    , ("statusCode",      JSON.toJSON $ HTTP.statusCode statusCode)
    ]
    where
      headersJSON
        = JSON.Object . KeyMap.fromList
        $ bimap (JSON.Key.fromText . CI.original) JSON.String <$> headers

data ResponseBody
  = ByteStringResponseBody BS.ByteString
  | TextResponseBody Text BS.ByteString

isByteString :: ResponseBody -> Bool
isByteString = \case
  ByteStringResponseBody{} -> True
  TextResponseBody{}       -> False

responseBodyContentLength :: ResponseBody -> Natural
responseBodyContentLength = \case
  (ByteStringResponseBody byteString) -> convertImpure $ BS.length byteString
  (TextResponseBody _text byteString) -> convertImpure $ BS.length byteString

responseBodyByteString :: ResponseBody -> BS.ByteString
responseBodyByteString = \case
  (ByteStringResponseBody byteString) -> byteString
  (TextResponseBody _text byteString) -> byteString

textBody :: ResponseBody -> Text
textBody = \case
  (TextResponseBody text _byteString) -> text
  (ByteStringResponseBody byteString) -> Text.decodeUtf8 (Base64.encode byteString)

mkByteStringResponseBody :: BS.ByteString -> ResponseBody
mkByteStringResponseBody = ByteStringResponseBody

mkTextResponseBody :: Text -> ResponseBody
mkTextResponseBody value = TextResponseBody value (Text.encodeUtf8 value)

run
  :: forall m . (MonadCatch m, MonadIO m)
  => (Request -> m Response)
  -> m ()
run lambdaFn = Lambda.Runtime.run $ \Lambda.Runtime.Event{..} -> do
  request <- liftIO $ parseRequest body
  JSON.toJSON <$> lambdaFn request

parseRequest
  :: forall m . MonadCatch m
  => JSON.Value
  -> m Request
parseRequest
  = either (throwM . RequestDecodeFailure . convert @Text) pure
  . JSON.parseEither JSON.parseJSON
