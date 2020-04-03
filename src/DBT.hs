{-# LANGUAGE AllowAmbiguousTypes #-}
module DBT where

-- Disclaimer, yes I should use singletons.
-- No I'll not block on re-learning them right now.

import DBT.Backend
import DBT.Prelude
import qualified DBT.Backend    as Backend
import qualified DBT.Postgresql as Postgresql

withDatabaseContainer :: MonadUnliftIO m => (Postgresql.ClientConfig -> m a) -> m a
withDatabaseContainer action = do
  implementation <- getImplementation
  case implementation of
    Podman -> Backend.withDatabaseContainer @'Podman action
    Docker -> Backend.withDatabaseContainer @'Docker action

getImplementation :: MonadIO m => m Implementation
getImplementation = do
  podman <- try @'Podman Podman
  docker <- try @'Docker Docker

  maybe
    (liftIO $ fail "Neither found podman nor docker dbt backends")
    pure
    (podman <|> docker)

try
  :: forall (b :: Implementation) m . (Backend b, MonadIO m)
  => Implementation
  -> m (Maybe Implementation)
try implementation = do
  isAvailable <- available @b
  if isAvailable
    then pure $ pure implementation
    else pure empty

