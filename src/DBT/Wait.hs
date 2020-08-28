{-# LANGUAGE RankNTypes #-}

module DBT.Wait (Config(..), wait) where

import Control.Concurrent (threadDelay)
import DBT.Connection
import DBT.Prelude
import Data.Int (Int)
import GHC.Enum (succ)
import GHC.Real (fromIntegral)

import qualified DBT.Postgresql   as Postgresql
import qualified Hasql.Connection as Hasql

data Config = Config
  { clientConfig :: Postgresql.ClientConfig
  , maxAttempts  :: Natural
  , onFail       :: forall m . MonadIO m => m ()
  , prefix       :: String
  , printStatus  :: forall a m . (ToText a, MonadIO m) => a -> m ()
  , waitTime     :: Natural
  }

wait :: MonadIO m => Config -> m ()
wait Config{clientConfig = clientConfig@Postgresql.ClientConfig{..}, ..} = liftIO $
  start =<< effectiveWaitTime
  where
    failPrefix :: String -> IO a
    failPrefix message = fail $ prefix <> (' ':message)

    effectiveWaitTime :: IO Int
    effectiveWaitTime =
      if waitTime <= fromIntegral (maxBound @Int)
        then pure $ fromIntegral waitTime
        else failPrefix $ "Cannot convert waitTime: " <> show waitTime <> " to Int"

    start :: Int -> IO ()
    start waitTime' = attempt 1
      where
        attempt :: Natural -> IO ()
        attempt count =
          either (onError count) pure =<< (liftIO . withConnectionEither clientConfig $ const $ pure ())

        onError :: Natural -> Hasql.ConnectionError -> IO ()
        onError attempts error =
          if attempts == maxAttempts
            then do
              onFail
              failPrefix $ "Giving up connection, last error: " <> show error
            else do
              printStatus $ "Retrying failed connection attempt from error: " <> show error
              threadDelay waitTime'
              attempt $ succ attempts
