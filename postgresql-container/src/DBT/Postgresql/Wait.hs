{-# LANGUAGE RankNTypes #-}

module DBT.Postgresql.Wait (Config(..), wait) where

import Control.Concurrent (threadDelay)
import DBT.Postgresql.Connection
import DBT.Postgresql.Prelude
import Data.Int (Int)
import GHC.Enum (succ)
import GHC.Real (fromIntegral)
import UnliftIO.Exception (throwString)

import qualified Colog
import qualified DBT.Postgresql   as Postgresql
import qualified Hasql.Connection as Hasql

data Config env = Config
  { clientConfig :: Postgresql.ClientConfig
  , maxAttempts  :: Natural
  , onFail       :: RIO env ()
  , prefix       :: String
  , waitTime     :: Natural
  }

wait :: forall env . Colog.WithLog env Colog.Message (RIO env) => Config env -> RIO env ()
wait Config{clientConfig = clientConfig, ..} =
  start =<< effectiveWaitTime
  where
    failPrefix :: String -> RIO env a
    failPrefix message = throwString $ prefix <> (' ':message)

    effectiveWaitTime :: RIO env Int
    effectiveWaitTime =
      if waitTime <= fromIntegral (maxBound @Int)
        then pure $ fromIntegral waitTime
        else failPrefix $ "Cannot convert waitTime: " <> show waitTime <> " to Int"

    start :: Int -> RIO env ()
    start waitTime' = attempt 1
      where
        attempt :: Natural -> RIO env ()
        attempt count =
          either (onError count) pure =<< (withConnectionEither clientConfig . const $ pure ())

        onError :: Natural -> Hasql.ConnectionError -> RIO env ()
        onError attempts error =
          if attempts == maxAttempts
            then do
              onFail
              failPrefix $ "Giving up connection, last error: " <> show error
            else do
              Colog.logDebug . convert $ "Retrying failed connection attempt from error: " <> show error
              liftIO $ threadDelay waitTime'
              attempt $ succ attempts
