module Network.HTTP.MClient
  ( ContentType (..)
  , DecodeError (..)
  , HasHTTP
  , HasMediaType (..)
  , HttpError (..)
  , StatusCode
  , mkRequest
  ) where

import Control.Arrow (left)
import Control.Monad.Catch (Exception, catch)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Data.Bounded.Integral (BoundNumber)
import Data.Conversions (Conversion, ToText, convertUnsafe, toText)
import Data.Conversions.FromType (fromType)
import GHC.Records (HasField(..))
import MPrelude
import Network.HTTP.Media (MediaType, (//), (/:), matches, parseAccept)
import Network.HTTP.Types (hContentType, statusCode)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List                  as List
import qualified Data.List.NonEmpty         as NE
import qualified Network.HTTP.Client        as HTTP

type Response = HTTP.Response LBS.ByteString

type StatusCode = BoundNumber "Status code" '(199, 600)

type HasHTTP env = HasField "httpManager" env HTTP.Manager

newtype DecodeError = DecodeError Text
  deriving (Conversion Text) via Text
  deriving stock (Eq, Show)

data HttpError =
    ConnectionError HTTP.HttpException
  | DecodeFailure DecodeError Response
  | InvalidStatusCode StatusCode Response
  | InvalidContentType Response
  | UnsupportedContentType MediaType Response
  deriving stock (Show)

instance Eq HttpError where
  DecodeFailure t r          == DecodeFailure t' r'          = t == t' && r == r'
  InvalidStatusCode code r   == InvalidStatusCode code' r'   = r == r' && code == code'
  InvalidContentType r       == InvalidContentType r'        = r == r'
  UnsupportedContentType m r == UnsupportedContentType m' r' = m == m' && r == r'
  _                          == _                            = False

instance Exception HttpError

data ContentType
  = Json
  | PlainText
  | XML

class HasMediaType (ctyp :: ContentType) where
  mediaType :: MediaType
  mediaType = NE.head (mediaTypes @ctyp)

  mediaTypes :: NE.NonEmpty MediaType
  mediaTypes = mediaType @ctyp NE.:| []

instance HasMediaType 'Json where
  mediaType = "application" // "json" /: ("charset", "utf-8")

instance HasMediaType 'PlainText where
  mediaTypes
    = "text" // "plain" /: ("charset", "utf-8")
    NE.:|
    [ "text" // "html" /: ("charset", "utf-8")]

mkRequest
  :: forall ctyp e a. (ToText e, HasMediaType ctyp)
  => HTTP.Manager
  -> (LBS.ByteString -> Either e a)
  -> HTTP.Request
  -> IO (Either HttpError a)
mkRequest httpManager decodeBody =
  runExceptT . mkRequest' @ctyp (fromType @200) httpManager decodeBody

mkRequest'
  :: forall ctyp a e. (ToText e, HasMediaType ctyp)
  => StatusCode
  -> HTTP.Manager
  -> (LBS.ByteString -> Either e a)
  -> HTTP.Request
  -> ExceptT HttpError IO a
mkRequest' expectedStatus httpManager decodeBody request = do
  response <- ExceptT . catchConnectionError $ HTTP.httpLbs request httpManager
  contentType <- liftEither $ getContentType response

  let accepts = mediaTypes @ctyp
  let code    = convertUnsafe @StatusCode
              . convertUnsafe @Natural
              . statusCode
              $ HTTP.responseStatus response

  if | code /= expectedStatus ->
        throwError $ InvalidStatusCode code response
     | not (List.any (`matches` contentType) accepts) ->
        throwError $ UnsupportedContentType contentType response
     | otherwise              -> liftEither
         . left (flip DecodeFailure response . DecodeError . toText)
         . decodeBody
         $ HTTP.responseBody response

catchConnectionError ::  IO a -> IO (Either HttpError a)
catchConnectionError action =
  catch (Right <$> action)
    $ \e -> pure . Left . ConnectionError $ (e :: HTTP.HttpException)

getContentType :: Response -> Either HttpError MediaType
getContentType response
  = maybe (Left $ InvalidContentType response) pure
  . maybe (pure $ mediaType @'PlainText) parseAccept
  . List.lookup hContentType
  $ HTTP.responseHeaders response