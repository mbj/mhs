-- | Model the XRay Segments that can be send to AWS/XRay
--
-- @see https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html
--
-- We model the XRay Segments API 1:1. Mostly because XRay places additional
-- constraints on the values that do not exist in their real world
-- counterparts, such as length limits on the http request URLs. For that
-- reason we duplicate types that exist (with less constraints) in native
-- libraries.
--
-- Users of these types should use save conversions (such as truncation for
-- length limited fields) to make sure their traces never get rejected.
module XRay.Segment
  ( AnnotationValue(..)
  , Annotations
  , Aws(..)
  , AwsAccountId
  , Cause(..)
  , CausePath
  , Ecs(..)
  , Exception(..)
  , ExceptionId(..)
  , ExceptionMessage
  , Http(..)
  , HttpClientIp
  , HttpRequest(..)
  , HttpResponse(..)
  , HttpUrl
  , HttpUserAgent
  , Namespace(..)
  , Origin(..)
  , Segment(..)
  , SegmentId(..)
  , SegmentName
  , SegmentType(..)
  , Service(..)
  , Sql(..)
  , SqlPreparation(..)
  , Timestamp(..)
  , User
  , addException
  , segmentIdParser
  , toException
  )
where

import Data.Attoparsec.Text (Parser)
import Data.Bifunctor (second)
import Data.ByteString.Builder (toLazyByteString, word64HexFixed)
import Data.Map.Strict (Map)
import Data.Scientific (Scientific, scientific)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.System (SystemTime(MkSystemTime, systemNanoseconds, systemSeconds))
import Data.Word (Word64)
import GHC.Real (fromIntegral)
import GHC.TypeLits (Symbol)
import System.Random (Random)
import XRay.JSON
import XRay.Parser
import XRay.Prelude
import XRay.TraceId

import qualified Data.Aeson.KeyMap   as KeyMap
import qualified Data.Aeson.Types    as JSON
import qualified Data.Text           as Text
import qualified Network.HTTP.Types  as HTTP
import qualified UnliftIO.Exception  as Exception

-- | XRayString type specific to XRay restrictions
--
-- > Values are strings up to 250 characters unless noted otherwise.
type XRayString (a :: Symbol) = BoundText' a '(1, 250)

-- | Timestamp used for XRay segments
newtype Timestamp = Timestamp SystemTime
  deriving stock Show

instance JSON.ToJSON Timestamp where
  toJSON (Timestamp MkSystemTime{..}) = JSON.toJSON $ seconds + nanoseconds
    where
      seconds :: Scientific
      seconds = fromIntegral systemSeconds

      nanoseconds :: Scientific
      nanoseconds = scientific (fromIntegral systemNanoseconds) (-9)

-- | Segment that is part of a trace
--
-- @see https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-fields
--
-- @see https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-errors
--
-- > Multiple types can apply if errors compound. For example, a 429 Too Many Requests error from a downstream call may cause your application to return 500 Internal Server Error, in which case all three types would apply.
data Segment = Segment
  { annotations :: Maybe Annotations
  , aws         :: Maybe Aws
  , cause       :: Maybe Cause
  , endTime     :: Maybe Timestamp
  , error       :: Bool  -- ^ Client error occured
  , fault       :: Bool  -- ^ Server error occured
  , http        :: Maybe Http
  , id          :: SegmentId
  , inProgress  :: Bool
  , name        :: SegmentName
  , namespace   :: Maybe Namespace
  , origin      :: Maybe Origin
  , parentId    :: Maybe SegmentId
  , service     :: Maybe Service
  , sql         :: Maybe Sql
  , startTime   :: Timestamp
  , throttle    :: Bool  -- ^ Request was throttled
  , traceId     :: TraceId
  , type'       :: Maybe SegmentType
  , user        :: Maybe User
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON Segment where
  toJSON = JSON.genericToJSON jsonOptions

-- | Logical name of the service that handled the request
--
-- > The logical name of the service that handled the request, up to 200 characters.
type SegmentName = BoundText' "SegmentName" '(1, 200)

data SegmentType = Subsegment
  deriving stock (Generic, Show)

instance JSON.ToJSON SegmentType where
  toJSON = \case
    Subsegment -> JSON.String "subsegment"

-- | Unique identifier of segments within a trace
newtype SegmentId = SegmentId Word64
  deriving newtype (Random)
  deriving stock   (Eq, Show)

instance Conversion Text SegmentId where
  convert (SegmentId word) = hexEncode word

instance JSON.ToJSON SegmentId where
  toJSON = JSON.toJSON . toText

segmentIdParser :: Parser SegmentId
segmentIdParser = SegmentId <$> fixedHex 16

-- | Recorded error / throttle or fault cause
data Cause = Cause
  { exceptions       :: [Exception]
  , paths            :: Maybe [CausePath]
  , workingDirectory :: Maybe CausePath
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON Cause where
  toJSON = JSON.genericToJSON jsonOptions

type CausePath = XRayString "CausePath"

-- | Recorded exception details
--
-- @see https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-errors
data Exception = Exception
  { cause     :: Maybe ExceptionId       -- ^ Id of the extensions parent that caused this exception
  , id        :: ExceptionId             -- ^ Trace unique exception id
  , message   :: Maybe ExceptionMessage  -- ^ Human readable message of the exception
  , remote    :: Maybe Bool              -- ^ Indicating error was caused by a downstream service
  , skipped   :: Maybe Natural           -- ^ Exceptions that where skipped between this exception and its child
  , stack     :: Maybe [StackFrame]      -- ^ Stack frames collected
  , truncated :: Maybe Natural           -- ^ Number of stack frames omitted
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON Exception where
  toJSON = JSON.genericToJSON jsonOptions

-- | Exception ID unique among segments in the trace
newtype ExceptionId = ExceptionId Word64
  deriving newtype (Random)
  deriving stock   (Eq, Show)

instance JSON.ToJSON ExceptionId where
  toJSON (ExceptionId word) = JSON.toJSON $ hexEncode word

type ExceptionMessage = XRayString "ExceptionMessage"

data StackFrame = StackFrame
  { label :: Maybe StackFrameLabel
  , line  :: Maybe Natural
  , path  :: Maybe StackFramePath
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON StackFrame where
  toJSON = JSON.genericToJSON jsonOptions

type StackFrameLabel = XRayString "StackFrameLabel"
type StackFramePath = XRayString "StackFramePath"

-- | XRayString that identifies the user wo send the request
type User = XRayString "User"

-- | HTTP Transaction data
--
-- Used to record a traced HTTP transaction
--
-- @see https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-http
data Http = Http
  { request  :: Maybe HttpRequest
  , response :: Maybe HttpResponse
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON Http where
  toJSON = JSON.genericToJSON jsonOptions

data HttpResponse = HttpResponse
  { contentLength :: Maybe Natural
  , status        :: Maybe HTTP.Status
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON HttpResponse where
  toJSON HttpResponse{..}
    = JSON.Object
    [ ("content_length", JSON.toJSON contentLength)
    , ("status",         JSON.toJSON (HTTP.statusCode <$> status))
    ]

data HttpRequest = HttpRequest
  { clientIp      :: Maybe HttpClientIp
  , method        :: Maybe HTTP.StdMethod
  , traced        :: Maybe Bool
  , url           :: Maybe HttpUrl
  , userAgent     :: Maybe HttpUserAgent
  , xForwardedFor :: Maybe Bool
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON HttpRequest where
  toJSON HttpRequest{..} = JSON.Object . KeyMap.fromList $
    fmap
      (second JSON.toJSON)
      [ ("client_ip",       JSON.toJSON <$> clientIp)
      , ("method",          methodJSON <$> method)
      , ("traced",          JSON.toJSON <$> traced)
      , ("url",             JSON.toJSON <$> url)
      , ("user_agent",      JSON.toJSON <$> userAgent)
      , ("x_forwarded_for", JSON.toJSON <$> xForwardedFor)
      ]
    where
      methodJSON :: HTTP.StdMethod -> JSON.Value
      methodJSON = JSON.toJSON . show

type HttpClientIp  = XRayString "HttpClientIp"
type HttpUrl       = XRayString "HttpUrl"
type HttpUserAgent = XRayString "HttpUserAgent"

-- | SQL Statement data
--
--  Used to record a traced SQL statement
--
-- @see https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-sql
data Sql = Sql
  { connectionString :: Maybe (XRayString "SqlConnectionString")
  , databaseType     :: Maybe (XRayString "SqlDatabaseType")
  , databaseVersion  :: Maybe (XRayString "SqlDatabaseType")
  , driverVersion    :: Maybe (XRayString "SqlDriverVersion")
  , preparation      :: Maybe SqlPreparation
  , sanitizedQuery   :: Maybe (XRayString "SqlSanitizedQuery")
  , url              :: Maybe (XRayString "SqlURL")
  , user             :: Maybe (XRayString "SqlUser")
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON Sql where
  toJSON = JSON.genericToJSON jsonOptions

data SqlPreparation = PreparedCall | PreparedStatement
  deriving stock (Show)

instance JSON.ToJSON SqlPreparation where
  toJSON = \case
    PreparedCall      -> JSON.String "call"
    PreparedStatement -> JSON.String "statement"

-- | Type of AWS resource running the XRay application
data Origin = EC2Instance | ECSContainer | ElasticBeanstalkEnvironment
  deriving stock Show

instance JSON.ToJSON Origin where
  toJSON EC2Instance                 = JSON.toJSON ("AWS::EC2::Instance" :: Text)
  toJSON ECSContainer                = JSON.toJSON ("AWS::ECS::Container" :: Text)
  toJSON ElasticBeanstalkEnvironment = JSON.toJSON ("AWS::ElasticBeanstalk::Environment" :: Text)

-- | Information about the running XRay Application
newtype Service = Service
  { version :: ServiceVersion  -- ^ Identify the version of XRay application
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON Service where
  toJSON = JSON.genericToJSON jsonOptions

type ServiceVersion = XRayString "ServiceVersion"

-- | Segment data detailing optional AWS interaction
data Aws = Aws
  { accountId :: Maybe AwsAccountId
  , ecs       :: Maybe Ecs
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON Aws where
  toJSON = JSON.genericToJSON jsonOptions

-- | Records of cross AWS account interaction
--
-- Records the AWS account ID for a *foreign* AWS Account.
type AwsAccountId = XRayString "AwsAccountId"

-- | ECS specific AWS metadata
newtype Ecs = Ecs
  { container :: EcsContainerId
  }
  deriving stock (Generic, Show)

instance JSON.ToJSON Ecs where
  toJSON = JSON.genericToJSON jsonOptions

type EcsContainerId = XRayString "EcsContainerId"

-- | Segment Annotations
--
-- @see https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html#api-segmentdocuments-annotations
type Annotations = Map (XRayString "AnnotationName") AnnotationValue

data AnnotationValue
  = AnnotationBool Bool
  | AnnotationNumber Scientific
  | AnnotationString (XRayString "Annotation")
  deriving stock Show

instance JSON.ToJSON AnnotationValue where
  toJSON = \case
    (AnnotationBool value)   -> JSON.toJSON value
    (AnnotationNumber value) -> JSON.toJSON value
    (AnnotationString value) -> JSON.toJSON value

data Namespace = NamespaceAWS | NamespaceRemote
  deriving stock Show

instance JSON.ToJSON Namespace where
  toJSON = \case
    NamespaceAWS    -> JSON.String "aws"
    NamespaceRemote -> JSON.String "remote"

addException :: Segment -> Exception -> Segment
addException segment exception = setCause segment $ causeAddException present
  where
    present :: Cause
    present = fromMaybe emptyCause segment.cause

    emptyCause :: Cause
    emptyCause = Cause
      { exceptions       = []
      , paths            = empty
      , workingDirectory = empty
      }

    causeAddException :: Cause ->  Cause
    causeAddException cause =
      cause { exceptions = exception : cause.exceptions }

setCause :: Segment -> Cause -> Segment
setCause Segment{..} cause' = Segment { cause = pure cause', .. }

toException :: ExceptionId -> Exception.SomeException -> Exception
toException id exception = do
  Exception
    { cause     = empty
    , message   = pure $ exceptionMessage exception
    , remote    = empty
    , skipped   = empty
    , stack     = empty
    , truncated = empty
    , ..
    }

exceptionMessage :: Exception.SomeException -> ExceptionMessage
exceptionMessage exception = convertImpure truncated
  where
    text      = convert @Text $ Exception.displayException exception
    truncated =
      if Text.length text <= 250
        then text
        else Text.take 250 text

hexEncode :: Word64 -> Text
hexEncode
  = decodeUtf8
  . convert
  . toLazyByteString
  . word64HexFixed
