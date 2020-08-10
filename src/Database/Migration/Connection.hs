module Database.Migration.Connection
  ( withConnection
  )
where

import Data.ByteString (ByteString)
import Database.Migration.Prelude
import Hasql.Connection (Connection)
import UnliftIO.Exception (bracket)

import qualified DBT.Postgresql   as Postgresql
import qualified Data.Maybe       as Maybe
import qualified Data.Text        as Text
import qualified Hasql.Connection

data Parameter = Parameter
  { name  :: Text
  , value :: Text
  }

instance ToText Parameter where
  toText Parameter{..} = name <> "=" <> toText (quote value)

withConnection
  :: forall m a . (MonadUnliftIO m)
  => Postgresql.ClientConfig
  -> (Connection -> m a)
  -> m a
withConnection config = bracket open close
  where
    open :: m Connection
    open = do
      eitherConnection <- liftIO . Hasql.Connection.acquire $ settings config
      either (liftIO . fail . show) pure eitherConnection

    close :: Connection -> m ()
    close = liftIO . Hasql.Connection.release

settings :: Postgresql.ClientConfig -> Hasql.Connection.Settings
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

render :: [Maybe Parameter] -> ByteString
render parameters
  = convert
  . Text.intercalate " "
  $ toText <$> Maybe.catMaybes parameters

quote :: ToText a => a -> String
quote text = "'" <> escape (convertText text) <> "'"
  where
    escape :: String -> String
    escape ""     = ""
    escape (x:xs) = case x of
      '\'' -> '\\' : x : suffix
      '\\' -> '\\' : x : suffix
      _    ->        x : suffix
      where
        suffix = escape xs
