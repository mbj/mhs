{-# LANGUAGE RankNTypes #-}

module DBT.Wait (Config(..), wait) where

import Control.Concurrent (threadDelay)
import DBT.ClientConfig
import DBT.Connection
import DBT.Prelude
import Data.Int (Int)
import GHC.Enum (succ)
import GHC.Real (fromIntegral)
import UnliftIO.Exception (throwString)

import qualified Hasql.Connection as Hasql
import qualified MIO.Log          as Log

data Config env = Config
  { clientConfig :: ClientConfig
  , maxAttempts  :: Natural
  , onFail       :: MIO env ()
  , prefix       :: String
  , waitTime     :: Natural
  }

wait :: forall env . Log.Env env => Config env -> MIO env ()
wait Config{clientConfig = clientConfig, ..} =
  start =<< effectiveWaitTime
  where
    failPrefix :: String -> MIO env a
    failPrefix message = throwString $ prefix <> (' ':message)

    effectiveWaitTime :: MIO env Int
    effectiveWaitTime =
      if waitTime <= fromIntegral (maxBound @Int)
        then pure $ fromIntegral waitTime
        else failPrefix $ "Cannot convert waitTime: " <> show waitTime <> " to Int"

    start :: Int -> MIO env ()
    start waitTime' = attempt 1
      where
        attempt :: Natural -> MIO env ()
        attempt count =
          either (onError count) pure =<< (withConnectionEither clientConfig . const $ pure ())

        onError :: Natural -> Hasql.ConnectionError -> MIO env ()
        onError attempts error =
          if attempts == maxAttempts
            then do
              onFail
              failPrefix $ "Giving up connection, last error: " <> show error
            else do
              Log.debug . convert $ "Retrying failed connection attempt from error: " <> show error
              liftIO $ threadDelay waitTime'
              attempt $ succ attempts
