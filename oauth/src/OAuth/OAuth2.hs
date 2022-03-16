module OAuth.OAuth2
  ( AuthCode(..)
  , AuthenticationResponse(..)
  , Config(..)
  , Env
  , authenticate
  , authorizationRequestUrl
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

  runHttpRequest tokenURL requestObject

runHttpRequest
  :: forall a b env . (HTTP.Env env, JSON.ToJSON a, JSON.FromJSON b)
  => Text
  -> a
  -> RIO env b
runHttpRequest tokenURL requestObject =
  eitherThrow =<< HTTP.send HTTP.decodeJSON =<< httpRequest tokenURL requestObject

httpRequest
  :: forall a env . (JSON.ToJSON a)
  => Text
  -> a
  -> RIO env HTTP.Request
httpRequest tokenURL requestObject = do
  baseRequest <- HTTP.parseRequest (convert tokenURL)

  pure . HTTP.setJSONBody requestObject $ baseRequest { HTTP.method = HTTP.methodPost }

oAuth2JsonOptions :: JSON.Options
oAuth2JsonOptions = JSON.defaultOptions
  { JSON.fieldLabelModifier = JSON.camelTo2 '_'
  }
