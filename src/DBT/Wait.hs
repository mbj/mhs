{-# LANGUAGE RankNTypes #-}

module DBT.Wait (Config(..), wait) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (unless)
import DBT.Prelude
import Data.Int (Int)
import GHC.Enum (succ)
import GHC.Real (fromIntegral)

import qualified Control.Exception as Exception
import qualified DBT.Postgresql    as Postgresql
import qualified Network.Socket    as Socket

data Config = Config
  { prefix      :: String
  , hostName    :: Postgresql.HostName
  , hostPort    :: Postgresql.HostPort
  , maxAttempts :: Natural
  , waitTime    :: Natural
  , printStatus :: forall a m . (ToText a, MonadIO m) => a -> m ()
  }

wait :: MonadIO m => Config -> m ()
wait Config{..} = liftIO $ do
  address <- resolve

  logPrefix
    $  "hostname:"
    <> convertText hostName
    <> " resolved to: "
    <> show address

  start address =<< effectiveWaitTime
  where
    failPrefix :: String -> IO a
    failPrefix message = fail $ prefix <> (' ':message)

    logPrefix :: String -> IO ()
    logPrefix message = printStatus $ prefix <> (' ':message)

    effectiveWaitTime :: IO Int
    effectiveWaitTime =
      if waitTime <= fromIntegral (maxBound @Int)
        then pure $ fromIntegral waitTime
        else failPrefix $ "Cannot convert waitTime: " <> show waitTime <> " to Int"

    start :: Socket.AddrInfo -> Int -> IO ()
    start address waitTime' = attempt 1
      where
        attempt :: Natural -> IO ()
        attempt count = if count == maxAttempts
          then giveUp
          else do
            success <- runConnectionTest
            unless success $ threadDelay waitTime' >> attempt (succ count)

        giveUp :: IO a
        giveUp
          = failPrefix
          $  "Giving up connecting to "
          <> show address
          <> " after "
          <> show maxAttempts
          <> " attempts with wait time "
          <> show waitTime
          <> "us between attempts"

        runConnectionTest :: IO Bool
        runConnectionTest =
          Exception.catch
            (Exception.bracket (open address) Socket.close (const success))
            connectFailed
          where
            success :: IO Bool
            success = do
              logPrefix "connection success"
              pure True

            connectFailed :: SomeException -> IO Bool
            connectFailed exception = do
              logPrefix $ "TCP connect failed " <> show exception
              pure False

    resolve = listToMaybe <$> getAddrInfo >>= maybe failLookup pure
      where
        getAddrInfo =
          Socket.getAddrInfo
            (pure hints)
            (pure $ convertText hostName)
            (pure $ convertText hostPort)

        hints = Socket.defaultHints
          { Socket.addrSocketType = Socket.Stream
          , Socket.addrFamily     = Socket.AF_INET
          }

        failLookup :: IO a
        failLookup = failPrefix $ "Could not lookup hostname: " <> convertText hostName

open :: Socket.AddrInfo -> IO Socket.Socket
open Socket.AddrInfo{..} = do
  socket <- Socket.socket addrFamily addrSocketType addrProtocol
  Socket.connect socket addrAddress
  pure socket


