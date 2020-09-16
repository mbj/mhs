{-# LANGUAGE RankNTypes #-}

module DBT.Wait (Config(..), wait) where

import Control.Concurrent (threadDelay)
import DBT.Connection
import DBT.Prelude
import Data.Int (Int)
import GHC.Enum (succ)
import GHC.Real (fromIntegral)

import qualified CBT.Environment  as CBT
import qualified Colog
import qualified DBT.Postgresql   as Postgresql
import qualified Hasql.Connection as Hasql

data Config = Config
  { clientConfig :: Postgresql.ClientConfig
  , maxAttempts  :: Natural
  , onFail       :: forall m . CBT.HasEnvironment m => m ()
  , prefix       :: String
  , waitTime     :: Natural
  }

wait :: forall m . CBT.HasEnvironment m => Config -> m ()
wait Config{clientConfig = clientConfig@Postgresql.ClientConfig{..}, ..} =
  start =<< effectiveWaitTime
  where
    failPrefix :: String -> m a
    failPrefix message = liftIO $ fail $ prefix <> (' ':message)

    effectiveWaitTime :: m Int
    effectiveWaitTime =
      if waitTime <= fromIntegral (maxBound @Int)
        then pure $ fromIntegral waitTime
        else failPrefix $ "Cannot convert waitTime: " <> show waitTime <> " to Int"

    start :: Int -> m ()
    start waitTime' = attempt 1
      where
        attempt :: Natural -> m ()
        attempt count =
          either (onError count) pure =<< (liftIO . withConnectionEither clientConfig $ const $ pure ())

        onError :: Natural -> Hasql.ConnectionError -> m ()
        onError attempts error =
          if attempts == maxAttempts
            then do
              onFail
              failPrefix $ "Giving up connection, last error: " <> show error
            else do
              Colog.logDebug . convert $ "Retrying failed connection attempt from error: " <> show error
              liftIO $ threadDelay waitTime'
              attempt $ succ attempts
