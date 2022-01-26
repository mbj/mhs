module AWS.Lambda.Header where

import AWS.Lambda.Runtime.Prelude
import Data.HashMap.Strict (HashMap)

import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Text.Encoding   as Text
import qualified Network.HTTP.Types   as HTTP

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
