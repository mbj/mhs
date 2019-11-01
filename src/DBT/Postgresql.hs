module DBT.Postgresql
  ( Env(..)
  , Hostname(..)
  , Password(..)
  , Port(..)
  , Username(..)
  , mkEnv
  )
where

import DBT.Prelude

newtype Username = Username Text
  deriving newtype ToText

newtype Password = Password Text
  deriving newtype ToText

newtype Hostname = Hostname Text
  deriving newtype ToText

newtype Port = Port Text
  deriving newtype ToText

data Env = Env
  { hostPort       :: Port
  , hostname       :: Hostname
  , masterPassword :: Password
  , masterUsername :: Username
  }

mkEnv :: Env -> [(String, String)]
mkEnv Env{..} =
  [ ("PGHOST",     convertText hostname)
  , ("PGPORT",     convertText hostPort)
  , ("PGUSER",     convertText masterUsername)
  , ("PGDATABASE", convertText masterUsername)
  , ("PGPASSWORD", convertText masterPassword)
  ]
