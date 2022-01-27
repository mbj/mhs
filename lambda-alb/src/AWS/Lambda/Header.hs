module AWS.Lambda.Header where

import AWS.Lambda.Runtime.Prelude
import Data.HashMap.Strict (HashMap)

import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict  as HashMap

type HeaderName = CI.CI Text

type Header = (HeaderName, Text)

newtype Headers = Headers [Header]
  deriving stock (Eq, Show)

instance JSON.ToJSON Headers where
  toJSON (Headers list)
    = JSON.Object . HashMap.fromList
    $ bimap CI.original JSON.String <$> list

instance JSON.FromJSON Headers where
  parseJSON = JSON.withObject "Headers" parseHeaders
    where
      parseHeaders :: HashMap Text JSON.Value -> JSON.Parser Headers
      parseHeaders xs = Headers <$> traverse toHeader (HashMap.toList xs)

      toHeader :: (Text, JSON.Value) -> JSON.Parser Header
      toHeader (headerName, value) = case value of
        JSON.String text -> pure (CI.mk headerName, text)
        _                -> fail $ "Failure parsing header " <> convert headerName <> " : " <> show value


hAccept,hAuthorization, hCacheControl, hContentType, hOrigin :: HeaderName
hAccept        = "Accept"
hAuthorization = "Authorization"
hCacheControl  = "Cache-Control"
hContentType   = "Content-Type"
hOrigin        = "Origin"
