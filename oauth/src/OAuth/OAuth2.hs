module OAuth.OAuth2
  ( AuthCode(..)
  , AuthenticationResponse(..)
  , Config(..)
  , Env
  , RefreshAccessTokenResponse(..)
  , RefreshToken(..)
  , authenticate
  , authorizationRequestUrl
  , refreshAccessToken
  )
where

import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import OAuth.Prelude
import Prelude(Integer)

import qualified Crypto.Nonce               as Nonce
import qualified Data.Aeson                 as JSON
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.MClient       as HTTP
import qualified Network.HTTP.Types         as HTTP
import qualified Network.HTTP.Types.URI     as URI

data Config = Config
  { authURL      :: Text
  , clientId     :: Text
  , clientSecret :: Text
  , tokenURL     :: Text
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Config where
  parseJSON = JSON.genericParseJSON oAuth2JsonOptions

type Env env
  = ( HasField "oauthConfig" env Config
    , HTTP.Env env
    )

newtype AuthCode = AuthCode Text
  deriving newtype (JSON.ToJSON, JSON.FromJSON)
  deriving stock (Eq, Generic, Show)

newtype RefreshToken = RefreshToken Text
  deriving newtype (JSON.ToJSON, JSON.FromJSON)
  deriving stock (Eq, Show)

data AuthenticationRequest = AuthenticationRequest
  { clientId     :: Text
  , clientSecret :: Text
  , code         :: AuthCode
  , grantType    :: Text
  , nonce        :: Text
  , redirectUri  :: Text
  , state        :: [(Text, Text)]
  }
  deriving stock (Eq, Generic, Show)

instance JSON.ToJSON AuthenticationRequest where
  toJSON     = JSON.genericToJSON oAuth2JsonOptions
  toEncoding = JSON.genericToEncoding oAuth2JsonOptions

instance JSON.FromJSON AuthenticationRequest where
  parseJSON = JSON.genericParseJSON oAuth2JsonOptions

data AuthenticationResponse = AuthenticationResponse
  { accessToken  :: Text
  , expiresIn    :: Integer
  , idToken      :: Text
  , refreshToken :: Maybe Text
  , scope        :: Text
  , tokenType    :: Text
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON AuthenticationResponse where
  parseJSON = JSON.genericParseJSON oAuth2JsonOptions

data RefreshAccessTokenRequest = RefreshAccessTokenRequest
  { clientId     :: Text
  , clientSecret :: Text
  , grantType    :: Text
  , refreshToken :: RefreshToken
  }
  deriving stock (Eq, Generic, Show)

instance JSON.ToJSON RefreshAccessTokenRequest where
  toJSON     = JSON.genericToJSON oAuth2JsonOptions
  toEncoding = JSON.genericToEncoding oAuth2JsonOptions

instance JSON.FromJSON RefreshAccessTokenRequest where
  parseJSON = JSON.genericParseJSON oAuth2JsonOptions

data RefreshAccessTokenResponse = RefreshAccessTokenResponse
  { accessToken :: Text
  , expiresIn   :: Integer
  , scope       :: Text
  , tokenType   :: Text
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON RefreshAccessTokenResponse where
  parseJSON = JSON.genericParseJSON oAuth2JsonOptions

authorizationRequestUrl :: Env env => [Text] -> Text -> RIO env Text
authorizationRequestUrl scopes redirectUri = do
  config <- asks (getField @"oauthConfig")
  pure $ getField @"authURL" config <> Text.decodeUtf8 (query config)
  where
    query :: Config -> ByteString
    query config = URI.renderSimpleQuery True $ second Text.encodeUtf8 <$>
      [ ("access_type",   "offline")
      , ("client_id",     getField @"clientId" config)
      , ("redirect_uri",  redirectUri)
      , ("response_type", "code")
      , ("scope",         Text.intercalate " " $ ["openid", "email"] <> scopes)
      ]

authenticate
  :: forall env . Env env
  => AuthCode
  -> Text
  -> RIO env AuthenticationResponse
authenticate code redirectUri = do
  Config{..} <- asks (getField @"oauthConfig")
  nonce      <- liftIO $ Nonce.withGenerator Nonce.nonce128urlT

  let requestObject =
        AuthenticationRequest
          { grantType   = "authorization_code"
          , redirectUri = redirectUri
          , state       = []
          , ..
          }

  sendRequest >>= runHttpRequest tokenURL requestObject >>= eitherThrow

refreshAccessToken
  :: forall env . Env env
  => RefreshToken
  -> RIO env RefreshAccessTokenResponse
refreshAccessToken refreshToken = do
  Config{..} <- asks (getField @"oauthConfig")

  let requestObject =
        RefreshAccessTokenRequest
          { grantType = "refresh_token"
          , ..
          }

  sendRequest >>= runHttpRequest tokenURL requestObject >>= eitherThrow


runHttpRequest
  :: forall a b env . (JSON.ToJSON a, JSON.FromJSON b)
  => Text
  -> a
  -> HTTP.SendRequest
  -> RIO env (Either HTTP.HttpError b)
runHttpRequest tokenURL requestObject sendRequest' = liftIO $
  HTTP.mkRequest @'HTTP.Json @'HTTP.Json sendRequest' JSON.eitherDecode =<< httpRequest tokenURL requestObject

sendRequest :: forall env . HTTP.Env env => RIO env HTTP.SendRequest
sendRequest = asks $ getField @"httpSendRequest"

httpRequest
  :: forall a . (JSON.ToJSON a)
  => Text
  -> a
  -> IO HTTP.Request
httpRequest tokenURL requestObject = do
  baseRequest <- HTTP.parseRequest (convert tokenURL)

  pure baseRequest
    { HTTP.method      = HTTP.methodPost
    , HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode requestObject
    }

oAuth2JsonOptions :: JSON.Options
oAuth2JsonOptions = JSON.defaultOptions
  { JSON.fieldLabelModifier = JSON.camelTo2 '_'
  }
