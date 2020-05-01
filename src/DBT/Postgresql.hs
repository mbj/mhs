{-# LANGUAGE TupleSections #-}

module DBT.Postgresql
  ( ClientConfig(..)
  , DatabaseName(..)
  , HostName(..)
  , HostPort(..)
  , Password(..)
  , SSLMode(..)
  , SSLRootCert(..)
  , UserName(..)
  , defaultHostPort
  , effectiveHostPort
  , parseHostPort
  , toEnv
  )
where

import DBT.Prelude
import Data.Maybe (catMaybes)
import Data.Word (Word16)
import Text.Read (readMaybe)
import Text.Show (show)

newtype DatabaseName = DatabaseName Text
  deriving newtype ToText
  deriving stock Show

newtype UserName = UserName Text
  deriving newtype ToText
  deriving stock Show

newtype Password = Password Text
  deriving newtype ToText

instance Show Password where
  show _password = "(Password [redacted])"

newtype HostName = HostName Text
  deriving newtype ToText
  deriving stock Show

newtype HostPort = HostPort { unPort :: Word16 }
  deriving stock Show

instance ToText HostPort where
  toText (HostPort port) = convertText $ show port

newtype SSLMode = SSLMode Text
  deriving newtype ToText
  deriving stock Show

newtype SSLRootCert = SSLRootCert Text
  deriving newtype ToText
  deriving stock Show

data ClientConfig = ClientConfig
  { databaseName :: DatabaseName
  , hostName     :: HostName
  , hostPort     :: Maybe HostPort
  , password     :: Maybe Password
  , sslMode      :: Maybe SSLMode
  , sslRootCert  :: Maybe SSLRootCert
  , userName     :: UserName
  }
  deriving stock Show

toEnv :: ClientConfig -> [(String, String)]
toEnv ClientConfig{..} = catMaybes
  [ optional "PGPASSWORD"    password
  , optional "PGPORT"        hostPort
  , optional "PGSSLMODE"     sslMode
  , optional "PGSSLROOTCERT" sslRootCert
  , required "PGDATABASE"    databaseName
  , required "PGHOST"        hostName
  , required "PGUSER"        userName
  ]
  where
    required :: ToText a => String -> a -> Maybe (String, String)
    required key field = pure (key, convertText field)

    optional :: ToText a => String -> Maybe a -> Maybe (String, String)
    optional key = maybe empty (pure . (key,) . convertText)

parseHostPort :: forall m a . (MonadIO m, ToText a, Show a) => a -> m HostPort
parseHostPort input = maybe failParse (pure . HostPort) . readMaybe $ convertText input
  where
    failParse :: m HostPort
    failParse = liftIO . fail $ "Cannot parse PostgresqlPort from input: " <> show input

defaultHostPort :: HostPort
defaultHostPort = HostPort 5432

effectiveHostPort :: ClientConfig -> HostPort
effectiveHostPort ClientConfig{..} = fromMaybe defaultHostPort hostPort
