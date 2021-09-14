module AWS.Lambda.Runtime.Types where

import AWS.Lambda.Runtime.Prelude

import qualified Data.Aeson      as JSON
import qualified Data.ByteString as BS

data Event = Event
  { body      :: JSON.Value
  , requestId :: RequestId
  , traceId   :: TraceId
  }
  deriving stock Show

newtype RequestId = RequestId Text
  deriving stock (Show)

newtype TraceId = TraceId Text
  deriving stock (Show)

instance Conversion BS.ByteString RequestId where
  convert (RequestId requestId) = encodeUtf8 requestId