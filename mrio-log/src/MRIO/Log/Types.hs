module MRIO.Log.Types where

import Control.Concurrent (ThreadId)
import Data.Time.Clock (UTCTime)
import MPrelude

data Message = Message
  { severity :: Severity
  , message  :: Text
  , threadId :: ThreadId
  , time     :: UTCTime
  }

data Severity
  = Debug
  | Info
  | Warn
  | Error
  deriving stock (Show, Eq, Ord, Enum, Bounded)
