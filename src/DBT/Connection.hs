module DBT.Connection
  ( withConnection
  , withConnectionEither
  , withConnectionSession
  )
where

import DBT.Prelude
import Data.Functor (fmap)

import qualified DBT.Postgresql     as Postgresql
import qualified Data.ByteString    as BS
import qualified Data.Maybe         as Maybe
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Hasql.Connection   as Hasql
import qualified Hasql.Session      as Hasql
import qualified UnliftIO.Exception as Exception

withConnection
  :: forall m a . (MonadUnliftIO m)
  => Postgresql.ClientConfig
  -> (Hasql.Connection -> m a)
  -> m a
withConnection config =
  either (liftIO . fail . show) pure <=< withConnectionEither config

withConnectionEither
  :: forall m a . (MonadUnliftIO m)
  => Postgresql.ClientConfig
  -> (Hasql.Connection -> m a)
  -> m (Either Hasql.ConnectionError a)
withConnectionEither config action = do
  (liftIO . Hasql.acquire $ settings config)
     >>= either (pure . Left) (fmap Right . handleAction)
  where
    handleAction :: Hasql.Connection -> m a
    handleAction connection =
      Exception.finally
        (action connection)
        (liftIO $ Hasql.release connection)

withConnectionSession
  :: forall m a . (MonadUnliftIO m)
  => Postgresql.ClientConfig
  -> Hasql.Session a
  -> m a
withConnectionSession config session
  = withConnection config
  $ liftIO . (eitherFail <=< Hasql.run session)

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

instance ToText Parameter where
  toText Parameter{..} = name <> "='" <> toText (escape $ convertText value) <> "'"
    where
      escape []     = []
      escape (x:xs) = case x of
        '\'' -> '\\' : x : suffix
        '\\' -> '\\' : x : suffix
        _    ->        x : suffix
        where
          suffix = escape xs

eitherFail :: (MonadFail m, Show e) => Either e a -> m a
eitherFail = either (fail . show) pure
