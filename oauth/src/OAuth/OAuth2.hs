{-# LANGUAGE TupleSections #-}

module OAuth.OAuth2
  ( AuthCode
  , AuthorizationRequest(..)
  , Config(..)
  , Env
  , RedirectURI
  , State
  , TokenRequest(..)
  , TokenResponse(..)
  , authorizationRequestURI
  , getAuthorizationRequest
  , requestToken
  )
where

import Control.Monad (replicateM)
import Data.Bifunctor (second)
import Data.Bounded
import Data.ByteString (ByteString)
import Data.Word (Word8)
import OAuth.Prelude
import Prelude(Integer)

import qualified Data.Aeson             as JSON
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Network.HTTP.Client    as HTTP
import qualified Network.HTTP.MClient   as HTTP
import qualified Network.HTTP.Types.URI as URI
import qualified System.Random          as System

type AccessType            = BoundText "OAuth2 AccessType"
type AuthCode              = BoundText "OAuth2 AuthCode"
type AuthorizationEndpoint = BoundText "OAuth2 AuthorizationEndpoint"
type ClientId              = BoundText "OAuth2 ClientId"
type ClientSecret          = BoundText "OAuth2 ClientSecret"
type GrantType             = BoundText "OAuth2 GrantType"
type RedirectURI           = BoundText "OAuth2 RedirectURI"
type ResponseType          = BoundText "OAuth2 ResponseType"
type Scope                 = BoundText "OAuth2 Scope"
type State                 = BoundText "OAuth2 State"
type TokenEndpoint         = BoundText "OAuth2 TokenEndpoint"

data Config = Config
  { authorizationEndpoint :: AuthorizationEndpoint
  , clientId              :: ClientId
  , clientSecret          :: ClientSecret
  , tokenEndpoint         :: TokenEndpoint
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Config where
  parseJSON = JSON.genericParseJSON oAuth2JsonOptions

type Env env
  = ( HasField "oauthConfig" env Config
    , HTTP.Env env
    )

data TokenRequest = TokenRequest
  { code          :: Maybe AuthCode
  , grantType     :: Maybe GrantType
  , redirectURI   :: Maybe RedirectURI
  }
  deriving stock (Eq, Generic, Show)

instance JSON.ToJSON TokenRequest where
  toJSON     = JSON.genericToJSON oAuth2JsonOptions
  toEncoding = JSON.genericToEncoding oAuth2JsonOptions

data TokenResponse = TokenResponse
  { accessToken  :: Text
  , expiresIn    :: Integer
  , idToken      :: Text
  , refreshToken :: Maybe Text
  , scope        :: Text
  , tokenType    :: Text
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON TokenResponse where
  parseJSON = JSON.genericParseJSON oAuth2JsonOptions

data AuthorizationRequest = AuthorizationRequest
  { authorizationEndpoint :: AuthorizationEndpoint
  , accessType            :: Maybe AccessType
  , clientId              :: ClientId
  , redirectURI           :: RedirectURI
  , responseType          :: ResponseType
  , scopes                :: [Scope]
  , state                 :: State
  }

getAuthorizationRequest
  :: Config
  -> RedirectURI
  -> ResponseType
  -> [Scope]
  -> MIO env AuthorizationRequest
getAuthorizationRequest Config{..} redirectURI responseType scopes = do
  state <- convertImpure <$> getRandomText
  pure AuthorizationRequest{accessType = empty, ..}

authorizationRequestURI :: AuthorizationRequest -> Text
authorizationRequestURI AuthorizationRequest{..} =
  convert authorizationEndpoint <> Text.decodeUtf8 query
  where
    query :: ByteString
    query = URI.renderSimpleQuery True $ second Text.encodeUtf8 <$>
      [ ("client_id",     convert clientId)
      , ("redirect_uri",  convert redirectURI)
      , ("response_type", convert responseType)
      , ("scope",         Text.intercalate " " $ convert <$> scopes)
      , ("state",         convert state)
      ]
      <> optional "access_type" (convert @Text <$> accessType)

    optional :: BS.ByteString -> Maybe Text -> [(BS.ByteString, Text)]
    optional key = maybe [] (pure . (key,))

requestToken
  :: HTTP.Env env
  => Config
  -> TokenRequest
  -> MIO env TokenResponse
requestToken Config{..} TokenRequest{..} = do
  baseRequest <- HTTP.parseRequest (convert @String $ convert @Text tokenEndpoint)

  eitherThrow =<< HTTP.send HTTP.decodeJSONOk (HTTP.urlEncodedBody pairs baseRequest)
  where
    pairs :: [(BS.ByteString, BS.ByteString)]
    pairs
      =  [ ("client_id",     encodeUtf8 clientId)
         , ("client_secret", encodeUtf8 clientSecret)
         ]
      <> optional "code"         code
      <> optional "grant_type"   grantType
      <> optional "redirect_uri" redirectURI

    optional :: Conversion Text a => BS.ByteString -> Maybe a -> [(BS.ByteString, BS.ByteString)]
    optional key = maybe [] (pure . (key,) . encodeUtf8)

oAuth2JsonOptions :: JSON.Options
oAuth2JsonOptions = JSON.defaultOptions
  { JSON.fieldLabelModifier = JSON.camelTo2 '_'
  }

getRandomText :: MIO env Text
getRandomText = liftIO $ Text.decodeUtf8 . Base64.encode . BS.pack <$> replicateM 32 (System.randomIO @Word8)
