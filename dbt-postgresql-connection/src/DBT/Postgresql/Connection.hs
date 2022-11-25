module DBT.Postgresql.Connection
  ( Env
  , runSession
  , runSessionEither
  , withConnection
  , withConnectionEither
  , withConnectionSession
  )
where

import Control.Monad.Reader (asks)
import DBT.Postgresql.Prelude

import qualified DBT.Postgresql     as Postgresql
import qualified Data.ByteString    as BS
import qualified Data.Maybe         as Maybe
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Hasql.Connection   as Hasql
import qualified Hasql.Session      as Hasql
import qualified UnliftIO.Exception as Exception

type Env env = HasField "hasqlConnection" env Hasql.Connection

withConnection
  :: Postgresql.ClientConfig
  -> (Hasql.Connection -> RIO env a)
  -> RIO env a
withConnection config =
  either (Exception.throwString . show) pure <=< withConnectionEither config

runSession :: Env env => Hasql.Session a -> RIO env a
runSession = either Exception.throwIO pure <=< runSessionEither

runSessionEither :: Env env => Hasql.Session a -> RIO env (Either Hasql.QueryError a)
runSessionEither session = liftIO . Hasql.run session =<< asks (getField @"hasqlConnection")

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
