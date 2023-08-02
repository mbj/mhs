{-# LANGUAGE RankNTypes #-}

module XRay where

import Control.Monad.Reader (asks)
import MIO.Core
import System.Random (randomIO)
import XRay.Config
import XRay.Connection
import XRay.Prelude
import XRay.Segment
import XRay.TraceId

import qualified Data.Time.Clock.System as Clock
import qualified UnliftIO.Exception     as Exception

data Environment = Environment
  { config         :: Config
  , getTimestamp   :: IO Timestamp
  , newExceptionId :: IO ExceptionId
  , newSegmentId   :: IO SegmentId
  , newTraceId     :: IO TraceId
  , sendSegment    :: SendSegment
  }

type Env env = HasField "xrayEnvironment" env Environment

defaultEnvironment :: Config -> SendSegment -> Environment
defaultEnvironment config sendSegment =
  Environment
    { getTimestamp   = Timestamp <$> liftIO Clock.getSystemTime
    , newExceptionId = liftIO randomIO
    , newSegmentId   = liftIO randomIO
    , newTraceId     = liftIO newTraceIdIO
    , ..
    }

newSegment
  :: Env env
  => SegmentName
  -> TraceId
  -> Maybe SegmentId
  -> MIO env Segment
newSegment name traceId parentId = do
  Environment{config = Config{..}, ..} <- asks (.xrayEnvironment)

  liftIO $ do
    id        <- newSegmentId
    startTime <- getTimestamp

    pure $ Segment
      { cause      = empty
      , endTime    = empty
      , error      = False
      , fault      = False
      , http       = empty
      , inProgress = True
      , namespace  = empty
      , parentId   = parentId
      , sql        = empty
      , throttle   = False
      , type'      = empty
      , user       = empty
      , ..
      }

withSegment
  :: forall env a . Env env
  => SegmentName
  -> TraceId
  -> Maybe SegmentId
  -> (Segment -> Segment)
  -> (a -> Segment -> Segment)
  -> (SegmentId -> MIO env a)
  -> MIO env a
withSegment
  segmentName
  traceId
  parentId
  specializeInProgress
  specializeFinal
  action
  = recordSegment =<< asks (.xrayEnvironment)
  where
    recordSegment Environment{..} = do
      segment <- specializeInProgress <$> newSegment segmentName traceId parentId

      liftIO $ sendSegment segment

      Exception.tryAny (action segment.id)
        >>= either (finalizeException segment) (finalizeResult segment)
      where
        finalizeException :: Segment -> Exception.SomeException -> MIO env a
        finalizeException segment exception = do
          exceptionId <- liftIO newExceptionId
          finalize $ (addException segment (toException exceptionId exception)) { fault = True }
          Exception.throwIO exception

        finalizeResult :: Segment -> a -> MIO env a
        finalizeResult segment result =
          finalize (specializeFinal result segment) $> result

        finalize :: Segment -> MIO env ()
        finalize segment = do
          liftIO $ do
            endTime <- getTimestamp
            sendSegment $ segment { endTime = pure endTime, inProgress = False }
