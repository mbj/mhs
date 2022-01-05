module AWS.Lambda.ALB
  ( Headers(..)
  , Method(..)
  , Request(..)
  , Response(..)
  , ResponseBody
  , mkTextResponseBody
  , responseBodyByteString
  , responseBodyContentLength
  , run
  )
where

import AWS.Lambda.Runtime.Prelude
import Data.HashMap.Strict (HashMap)

import qualified AWS.Lambda.Runtime   as Lambda.Runtime
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.ByteString      as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Text.Encoding   as Text
import qualified Network.HTTP.Types   as HTTP

newtype RequestDecodeFailure = RequestDecodeFailure Text
  deriving anyclass (Exception)
  deriving stock (Eq, Show)

newtype Headers = Headers [HTTP.Header]
  deriving stock (Eq, Show)

instance JSON.ToJSON Headers where
  toJSON (Headers list)
    = JSON.Object . HashMap.fromList
    $ bimap (decodeUtf8 . CI.original) (JSON.String . decodeUtf8) <$> list

instance JSON.FromJSON Headers where
  parseJSON = JSON.withObject "Headers" parseHeaders
    where
      parseHeaders :: HashMap Text JSON.Value -> JSON.Parser Headers
      parseHeaders xs = Headers <$> traverse toHeader (HashMap.toList xs)

      toHeader :: (Text, JSON.Value) -> JSON.Parser HTTP.Header
      toHeader (headerName, value) = do
        headerValue <- JSON.withText
          ("Header Value for " <> convert headerName)
          (pure . Text.encodeUtf8)
          value
        pure (CI.mk $ Text.encodeUtf8 headerName, headerValue)

newtype Method = Method HTTP.StdMethod
  deriving stock (Show, Eq, Ord)

instance Conversion HTTP.StdMethod Method where
  convert (Method value) = value

instance JSON.ToJSON Method where
  toJSON (Method stdMethod)
    = JSON.String . decodeUtf8 $ HTTP.renderStdMethod stdMethod

instance JSON.FromJSON Method where
  parseJSON = JSON.withText "HTTP Method" parse
    where
      parse :: Text -> JSON.Parser Method
      parse
        = either (fail . show) (pure . Method)
        . HTTP.parseMethod
        . Text.encodeUtf8

data Request a = Request
  { path                  :: Text
  , httpMethod            :: Method
  , headers               :: Headers
  , queryStringParameters :: HashMap Text Text
  , isBase64Encoded       :: Bool
  , requestContext        :: JSON.Value
  , body                  :: a
  }
  deriving anyclass (JSON.FromJSON)
  deriving stock    (Generic, Show, Eq)

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
