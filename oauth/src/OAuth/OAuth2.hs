{-# OPTIONS -Werror #-}
module OAuth.OAuth2
  ( AuthCode
  , AuthorizationRequest(..)
  , Config(..)
  , Env
  , RedirectURI
  , State
  , TokenResponse(..)
  , authorizationRequestURI
  , getAuthorizationRequest
  , getTokenRequest
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
import qualified Network.HTTP.Types     as HTTP
import qualified Network.HTTP.Types.URI as URI
import qualified System.Random          as System

type AccessType            = BoundText "OAuth2 AccessType"
type AuthCode              = BoundText "OAuth2 AuthCode"
type AuthorizationEndpoint = BoundText "OAuth2 AuthorizationEndpoint"
type ClientId              = BoundText "OAuth2 ClientId"
type ClientSecret          = BoundText "OAuth2 ClientSecret"
type GrantType             = BoundText "OAuth2 GrantType"
type Nonce                 = BoundText "OAuth2 Nonce"
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
  { tokenEndpoint :: TokenEndpoint
  , clientId      :: ClientId
  , clientSecret  :: ClientSecret
  , code          :: AuthCode
  , grantType     :: GrantType
  , nonce         :: Nonce
  , redirectURI   :: RedirectURI
  , state         :: State
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
  , accessType            :: AccessType
  , clientId              :: ClientId
  , redirectURI           :: RedirectURI
  , responseType          :: ResponseType
  , scopes                :: [Scope]
  , state                 :: State
  }

getAuthorizationRequest
  :: Config
  -> AccessType
  -> RedirectURI
  -> ResponseType
  -> [Scope]
  -> RIO env AuthorizationRequest
getAuthorizationRequest Config{..} accessType redirectURI responseType scopes = do
  state <- convertImpure <$> getRandomText
  pure AuthorizationRequest{..}

authorizationRequestURI :: AuthorizationRequest -> Text
authorizationRequestURI AuthorizationRequest{..} =
  convert authorizationEndpoint <> Text.decodeUtf8 query
  where
    query :: ByteString
    query = URI.renderSimpleQuery True $ second Text.encodeUtf8 <$>
      [ ("access_type",   convert accessType)
      , ("client_id",     convert clientId)
      , ("redirect_uri",  convert redirectURI)
      , ("response_type", convert responseType)
      , ("scope",         Text.intercalate " " $ convert <$> scopes)
      , ("state",         convert state)
      ]

getTokenRequest
  :: Config
  -> AuthCode
  -> GrantType
  -> RedirectURI
  -> State
  -> RIO env TokenRequest
getTokenRequest Config{..} code grantType redirectURI state = do
  nonce <- convertImpure <$> getRandomText
  pure TokenRequest{..}

requestToken
  :: HTTP.Env env
  => TokenRequest
  -> RIO env TokenResponse
requestToken request@TokenRequest{..} = runHttpRequest (convert tokenEndpoint) request

runHttpRequest
  :: forall a b env . (HTTP.Env env, JSON.ToJSON a, JSON.FromJSON b)
  => Text
  -> a
  -> RIO env b
runHttpRequest url requestObject =
  eitherThrow =<< HTTP.send HTTP.decodeJSONOk =<< httpRequest url requestObject

httpRequest
  :: forall a env . (JSON.ToJSON a)
  => Text
  -> a
  -> RIO env HTTP.Request
httpRequest uri requestObject = do
  baseRequest <- HTTP.parseRequest (convert uri)

  pure . HTTP.setJSONBody requestObject $ baseRequest { HTTP.method = HTTP.methodPost }

oAuth2JsonOptions :: JSON.Options
oAuth2JsonOptions = JSON.defaultOptions
  { JSON.fieldLabelModifier = JSON.camelTo2 '_'
  }

getRandomText :: RIO env Text
getRandomText = liftIO $ Text.decodeUtf8 . Base64.encode . BS.pack <$> replicateM 32 (System.randomIO @Word8)
