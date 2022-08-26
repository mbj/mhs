module AWS.Lambda.ALB.Header where

import AWS.Lambda.Runtime.Prelude
import Data.Aeson.KeyMap (KeyMap)

import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Key       as JSON.Key
import qualified Data.Aeson.KeyMap    as KeyMap
import qualified Data.Aeson.Types     as JSON
import qualified Data.CaseInsensitive as CI

type HeaderName = CI.CI Text

type Header = (HeaderName, Text)

newtype Headers = Headers [Header]
  deriving stock (Eq, Show)

instance JSON.ToJSON Headers where
  toJSON (Headers list)
    = JSON.Object . KeyMap.fromList
    $ bimap (JSON.Key.fromText . CI.original) JSON.String <$> list

instance JSON.FromJSON Headers where
  parseJSON = JSON.withObject "Headers" parseHeaders
    where
      parseHeaders :: KeyMap JSON.Value -> JSON.Parser Headers
      parseHeaders xs = Headers <$> traverse toHeader (KeyMap.toList xs)

      toHeader :: (JSON.Key, JSON.Value) -> JSON.Parser Header
      toHeader (headerName, value) =
        case value of
          JSON.String text -> pure (CI.mk $ JSON.Key.toText headerName, text)
          _                -> fail $ "Failure parsing header " <> JSON.Key.toString headerName <> " : " <> show value


hAccept,hAuthorization, hCacheControl, hContentType, hOrigin :: HeaderName
hAccept        = "Accept"
hAuthorization = "Authorization"
hCacheControl  = "Cache-Control"
hContentType   = "Content-Type"
hOrigin        = "Origin"
