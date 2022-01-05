module AWS.Lambda.Runtime.Types where

import AWS.Lambda.Runtime.Prelude

import qualified Data.ByteString  as BS
import qualified XRay.TraceHeader as XRay

data Event a = Event
  { body        :: a
  , requestId   :: RequestId
  , traceHeader :: XRay.TraceHeader
  }
  deriving stock Show

newtype RequestId = RequestId Text
  deriving stock (Show)

instance Conversion BS.ByteString RequestId where
  convert (RequestId requestId) = encodeUtf8 requestId
