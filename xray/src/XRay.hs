{-# LANGUAGE RankNTypes #-}

module XRay where

import Control.Monad.Reader (asks)
import MRIO.Core
import System.Random (randomIO)
import XRay.Config
import XRay.Connection
import XRay.Prelude
import XRay.Segment
import XRay.TraceHeader
import XRay.TraceId

import qualified Data.Time.Clock.System as Clock

data Environment = Environment
  { config         :: Config
  , connection     :: Connection
  , getTimestamp   :: forall env . RIO env Timestamp
  , newExceptionId :: forall env . RIO env ExceptionId
  , newSegmentId   :: forall env . RIO env SegmentId
  , newTraceId     :: forall env . RIO env TraceId
  , sendSegment    :: forall env . Segment -> RIO env ()
  }

class Env env where
  environment :: env -> Environment

defaultEnvironment :: Config -> Connection -> Environment
defaultEnvironment config connection =
  Environment
    { getTimestamp   = Timestamp <$> liftIO Clock.getSystemTime
    , newExceptionId = liftIO randomIO
    , newSegmentId   = liftIO randomIO
    , newTraceId     = liftIO newTraceIdIO
    , sendSegment    = liftIO . send connection
    , ..
    }

startSegment :: Env env => TraceHeader -> SegmentName -> RIO env Segment
startSegment TraceHeader{..} name = do
  Environment{config = Config{..}, ..} <- asks environment
  id        <- newSegmentId
  startTime <- getTimestamp

  pure $ Segment
    { cause      = empty
    , endTime    = empty
    , error      = False
    , fault      = False
    , http       = empty
    , inProgress = True
    , throttle   = False
    , type'      = empty
    , user       = empty
    , ..
    }
