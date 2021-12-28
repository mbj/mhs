{-# LANGUAGE RankNTypes #-}

module XRay where

import Control.Monad.Reader (asks)
import GHC.Records (HasField)
import MRIO.Core
import System.Random (randomIO)
import XRay.Config
import XRay.Connection
import XRay.Prelude
import XRay.Segment
import XRay.TraceId

import qualified Data.Time.Clock.System as Clock

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
  -> RIO env Segment
newSegment name traceId parentId = do
  Environment{config = Config{..}, ..} <- asks (getField @"xrayEnvironment")

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
  :: Env env
  => SegmentName
  -> TraceId
  -> Maybe SegmentId
  -> (Segment -> Segment)
  -> (a -> Segment -> Segment)
  -> RIO env a
  -> RIO env a
withSegment
  segmentName
  traceId
  parentId
  specializeInProgress
  specializeFinal
  action
  = do
    Environment{..} <- asks (getField @"xrayEnvironment")

    segment <- specializeInProgress <$> newSegment segmentName traceId parentId

    liftIO $ sendSegment segment

    result  <- action

    liftIO $ do
      endTime <- getTimestamp

      sendSegment $ specializeFinal result segment
        { endTime    = pure endTime
        , inProgress = False
        }

      pure result
