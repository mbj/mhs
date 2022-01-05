module AWS.Lambda.ALB
  ( Headers(..)
  , Method(..)
  , Request(..)
  , Response(..)
  , run
  )
where

import AWS.Lambda.Runtime.Prelude
import Data.HashMap.Strict (HashMap)

import qualified AWS.Lambda.Runtime   as Lambda.Runtime
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
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
  { isBase64Encoded :: Bool
  , statusCode      :: Natural
  , headers         :: Headers
  , body            :: Text
  }
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)
  deriving stock    (Generic, Show, Eq)

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
