module DBT.Connection
  ( ConnectionEnv(..)
  , runConnectionEnv
  , withConnection
  , withConnectionEither
  , withConnectionEnv
  , withConnectionSession
  )
where

import Control.Monad.Reader (ask)
import DBT.Prelude

import qualified DBT.Postgresql     as Postgresql
import qualified Data.ByteString    as BS
import qualified Data.Maybe         as Maybe
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Hasql.Connection   as Hasql
import qualified Hasql.Session      as Hasql
import qualified UnliftIO.Exception as Exception

data ConnectionEnv env = ConnectionEnv
  { connection :: Hasql.Connection
  , parent     :: env
  }

runConnectionEnv
  :: RIO (ConnectionEnv env) a
  -> Hasql.Connection
  -> RIO env a
runConnectionEnv action connection = do
  parent <- ask
  runRIO ConnectionEnv{..} action

withConnection
  :: Postgresql.ClientConfig
  -> (Hasql.Connection -> RIO env a)
  -> RIO env a
withConnection config =
  either (Exception.throwString . show) pure <=< withConnectionEither config

withConnectionEnv
  :: Postgresql.ClientConfig
  -> RIO (ConnectionEnv env) a
  -> RIO env a
withConnectionEnv config = withConnection config . runConnectionEnv

withConnectionEither
  :: forall a env . Postgresql.ClientConfig
  -> (Hasql.Connection -> RIO env a)
  -> RIO env (Either Hasql.ConnectionError a)
withConnectionEither config action = do
  (liftIO . Hasql.acquire $ settings config)
     >>= either (pure . Left) (fmap Right . handleAction)
  where
    handleAction :: Hasql.Connection -> RIO env a
    handleAction connection =
      Exception.finally
        (action connection)
        (liftIO $ Hasql.release connection)

withConnectionSession
  :: forall a env . ()
  => Postgresql.ClientConfig
  -> Hasql.Session a
  -> RIO env a
withConnectionSession config session
  = withConnection config
  $ either Exception.throwIO pure <=< (liftIO . Hasql.run session)

settings :: Postgresql.ClientConfig -> Hasql.Settings
settings config@Postgresql.ClientConfig{..}
  = render
  [ required "dbname"   databaseName
  , required "host"     hostName
  , required "port"     (Postgresql.effectiveHostPort config)
  , required "user"     userName
  , optional "password" password
  , optional "sslmode"  sslMode
  ]
  where
    required :: ToText a => Text -> a -> Maybe Parameter
    required name = pure . Parameter name . toText

    optional :: ToText a => Text -> Maybe a -> Maybe Parameter
    optional name value = Parameter name . toText <$> value

render :: [Maybe Parameter] -> BS.ByteString
render parameters
  = Text.encodeUtf8
  . Text.intercalate " "
  $ toText <$> Maybe.catMaybes parameters

data Parameter = Parameter
  { name  :: Text
  , value :: Text
  }

instance Conversion Text Parameter where
  convert Parameter{..} = name <> "='" <> toText (escape $ convertText value) <> "'"
    where
      escape []     = []
      escape (x:xs) = case x of
        '\'' -> '\\' : x : suffix
        '\\' -> '\\' : x : suffix
        _    ->        x : suffix
        where
          suffix = escape xs
