module AWS.Lambda.ALB
  ( Headers(..)
  , Request(..)
  , Response(..)
  , ResponseBody
  , mkTextResponseBody
  , responseBodyByteString
  , responseBodyContentLength
  , run
  )
where

import AWS.Lambda.Header
import AWS.Lambda.Runtime.Prelude
import Data.Aeson ((.:))
import Data.HashMap.Strict (HashMap)

import qualified AWS.Lambda.Runtime   as Lambda.Runtime
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.ByteString      as BS
import qualified Data.Text.Encoding   as Text
import qualified Network.HTTP.Types   as HTTP

newtype RequestDecodeFailure = RequestDecodeFailure Text
  deriving anyclass (Exception)
  deriving stock (Eq, Show)

data Request a = Request
  { path                  :: Text
  , httpMethod            :: HTTP.StdMethod
  , headers               :: Headers
  , queryStringParameters :: HashMap Text Text
  , isBase64Encoded       :: Bool
  , requestContext        :: JSON.Value
  , body                  :: a
  }
  deriving stock (Generic, Show, Eq)

instance JSON.FromJSON a => JSON.FromJSON (Request a) where
  parseJSON = JSON.withObject "ALB Request" $ \object -> do
    path                  <- object .: "path"
    httpMethod            <- parseMethod =<< object .: "httpMethod"
    headers               <- object .: "headers"
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

data Response = Response
  { statusCode :: HTTP.Status
  , headers    :: Headers
  , body       :: ResponseBody
  }

instance JSON.ToJSON Response where
  toJSON Response{..}
    = JSON.Object
    [ ("body",            JSON.toJSON $ textBody body)
    , ("headers",         JSON.toJSON headers)
    , ("isBase64Encoded", JSON.toJSON False)
    , ("statusCode",      JSON.toJSON $ HTTP.statusCode statusCode)
    ]

data ResponseBody = TextResponseBody Text BS.ByteString

responseBodyContentLength :: ResponseBody -> Natural
responseBodyContentLength (TextResponseBody _text byteString) = convertUnsafe $ BS.length byteString

responseBodyByteString :: ResponseBody -> BS.ByteString
responseBodyByteString (TextResponseBody _text byteString) = byteString

textBody :: ResponseBody -> Text
textBody (TextResponseBody text _bytestring) = text

mkTextResponseBody :: Text -> ResponseBody
mkTextResponseBody value = TextResponseBody value (Text.encodeUtf8 value)

run
  :: forall body m . (MonadCatch m, MonadIO m, JSON.FromJSON body)
  => (Request body -> m Response)
  -> m ()
run lambdaFn = Lambda.Runtime.run $ \Lambda.Runtime.Event{..} -> do
  request <- liftIO $ parseRequest body
  JSON.toJSON <$> lambdaFn request

parseRequest
  :: forall m body. (MonadCatch m, JSON.FromJSON body)
  => JSON.Value
  -> m (Request body)
parseRequest
  = either (throwM . RequestDecodeFailure . convert @Text) pure
  . JSON.parseEither JSON.parseJSON
