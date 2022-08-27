module AWS.Lambda.ALB.Header where

import AWS.Lambda.Runtime.Prelude

import qualified Data.CaseInsensitive as CI

type Header     = (HeaderName, Text)
type HeaderName = CI.CI Text
type Headers    = [Header]

hAccept, hAuthorization, hCacheControl, hContentType, hOrigin :: HeaderName
hAccept        = "Accept"
hAuthorization = "Authorization"
hCacheControl  = "Cache-Control"
hContentType   = "Content-Type"
hOrigin        = "Origin"
