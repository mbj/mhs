module OAuth.OAuth2
  ( AuthenticationResponse(..)
  , AuthCode(..)
  , Credentials(..)
  , Env
  , OpenIdPayload(..)
  , RefreshAccessTokenResponse(..)
  , RefreshToken(..)
  , authenticate
  , authenticatedHttpRequest
  , authorizationRequestUrl
  , openIdPayload
  , refreshAccessToken
  )
where

import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Network.HTTP.MClient
import Network.URI (URI)
import OAuth.Prelude
import Prelude(Integer, (!!))

import qualified Crypto.Nonce               as Nonce
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Types         as HTTP
import qualified Network.HTTP.Types.URI     as URI

data Credentials = Credentials
  { authProviderX509CertUrl :: Text
  , authUri                 :: Text
  , clientId                :: Text
  , clientSecret            :: Text
  , projectId               :: Text
  , redirectUris            :: [Text]
  , tokenUri                :: Text
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON Credentials where
  parseJSON = JSON.genericParseJSON oAuth2JsonOptions

type Env env
  = ( HasField "googleOAuth2Credentials" env Credentials
    , HasHTTP env
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
  , refreshToken :: Text
  , scope        :: Text
  , tokenType    :: Text
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON AuthenticationResponse where
  parseJSON = JSON.genericParseJSON oAuth2JsonOptions

data OpenIdPayload = OpenIdPayload
  { email         :: Text
  , emailVerified :: Bool
  }
  deriving stock (Eq, Generic, Show)

instance JSON.FromJSON OpenIdPayload where
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

authorizationRequestUrl :: Credentials -> [Text] -> Text
authorizationRequestUrl Credentials{..} scopes =
  Text.decodeUtf8 $ "https://accounts.google.com/o/oauth2/v2/auth" <> query
  where
    query :: ByteString
    query = URI.renderSimpleQuery True $ second Text.encodeUtf8 <$>
      [ ("access_type",   "offline")
      , ("client_id",     clientId)
      , ("redirect_uri",  fromJust $ safeHead redirectUris)
      , ("response_type", "code")
      , ("scope",         Text.intercalate " " $ ["openid", "email"] <> scopes)
      ]

authenticate
  :: forall env . Env env
  => AuthCode
  -> RIO env AuthenticationResponse
authenticate code = do
  Credentials{..} <- asks (getField @"googleOAuth2Credentials")
  nonce           <- liftIO $ Nonce.withGenerator Nonce.nonce128urlT

  let requestObject =
        AuthenticationRequest
          { grantType   = "authorization_code"
          , redirectUri = "urn:ietf:wg:oauth:2.0:oob"
          , state       = []
          , ..
          }

  httpManager >>= runHttpRequest requestObject >>= eitherThrow

refreshAccessToken
  :: forall env . Env env
  => RefreshToken
  -> RIO env RefreshAccessTokenResponse
refreshAccessToken refreshToken = do
  Credentials{..} <- asks (getField @"googleOAuth2Credentials")

  let requestObject =
        RefreshAccessTokenRequest
          { grantType = "refresh_token"
          , ..
          }

  httpManager >>= runHttpRequest requestObject >>= eitherThrow


runHttpRequest
  :: forall a b env . (JSON.ToJSON a, JSON.FromJSON b)
  => a
  -> HTTP.Manager
  -> RIO env (Either HttpError b)
runHttpRequest requestObject manager = liftIO $
  mkRequest @'Json manager JSON.eitherDecode =<< httpRequest requestObject

httpManager :: forall env . HasHTTP env => RIO env HTTP.Manager
httpManager = asks $ getField @"httpManager"

httpRequest
  :: forall a . (JSON.ToJSON a)
  => a
  -> IO HTTP.Request
httpRequest requestObject = do
  baseRequest <- HTTP.parseRequest $ Text.unpack tokenEndpoint

  pure baseRequest
    { HTTP.method      = HTTP.methodPost
    , HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode requestObject
    }

authenticatedHttpRequest :: URI -> Text -> IO HTTP.Request
authenticatedHttpRequest uri accessToken = do
  baseRequest <- HTTP.requestFromURI uri

  pure baseRequest
    { HTTP.requestHeaders =
        [ (HTTP.hAuthorization, encodeUtf8 $ "Bearer " <> accessToken)
        ]
    }

openIdPayload :: Text -> Either String OpenIdPayload
openIdPayload idToken = case payloadJson of
    Left  msg  -> Left msg
    Right json -> JSON.eitherDecode $ BL.fromStrict json
  where
    payloadJson :: Either String ByteString
    payloadJson =
      Base64.decodeUnpadded $ encodeUtf8 (Text.splitOn "." idToken !! 1)

tokenEndpoint :: Text
tokenEndpoint = "https://oauth2.googleapis.com/token"

oAuth2JsonOptions :: JSON.Options
oAuth2JsonOptions = JSON.defaultOptions
  { JSON.fieldLabelModifier = JSON.camelTo2 '_'
  }
