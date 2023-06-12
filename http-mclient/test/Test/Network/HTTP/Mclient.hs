module Test.Network.HTTP.Mclient (testTree) where

import Control.Monad (when)
import Control.Monad.Reader (asks)
import Data.Word (Word8)
import GHC.Enum (succ)
import GHC.Records (HasField(..))
import MIO.Core
import MPrelude
import Test.Tasty
import Test.Tasty.HUnit ((@?=))

import qualified Data.Aeson               as JSON
import qualified Data.List                as List
import qualified MIO.Log                  as Log
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.MClient     as HTTP
import qualified Network.HTTP.Types       as HTTP
import qualified Network.Socket           as Network
import qualified Network.Wai              as WAI
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.Tasty.HUnit         as Tasty
import qualified UnliftIO
import qualified UnliftIO.Concurrent      as UnliftIO

data ExpectedResponse
  = ConnectionFailure
  | ResponseTimeout
  | Status500
  | Success
  deriving stock (Eq, Show)

data Environment = Environment
  { httpSendRequest :: HTTP.SendRequest
  , logAction       :: Log.Action
  , requestsCounter :: UnliftIO.MVar Word8
  }

testTree :: TestTree
testTree = testGroup "Network.HTTP.MClient"
  [ testGroup "with a connection failure"
      [ Tasty.testCase "return an HTTP connection failure after a single retry" .
          runWithEnvironment $ do
            (port, socket)  <- liftIO Warp.openFreePort

            assertRequestsCount 0

            UnliftIO.withAsync (runServer port socket) $ \server -> do
              UnliftIO.withAsync (sendHttpRequest port) $ \client -> do
                -- close server socket right before processing request
                liftIO $ Network.close socket

                assertResponse ConnectionFailure =<< UnliftIO.wait client

                -- assert 2 requests, where the second is a retry
                assertRequestsCount 2

                UnliftIO.cancel server
      ]
  , testGroup "with response timeout"
      [ Tasty.testCase "return an HTTP response timeout exception with no retries" .
          withServer application $ \port -> do
            assertRequestsCount 0

            UnliftIO.withAsync (sendHttpRequest' port setRequestTimeout identity) $ \client -> do
              assertResponse ResponseTimeout =<< UnliftIO.wait client

              assertRequestsCount 1
      ]
  , testGroup "with server response having an HTTP status"
      [ testGroup "with 500 HTTP status" . List.singleton .
          Tasty.testCase "return a 500 HTTP response with no retries" .
            withServer application $ \port -> do
              assertRequestsCount 0

              UnliftIO.withAsync (sendHttpRequest' port setRequest500 identity) $ \client -> do
                assertResponse Status500 =<< UnliftIO.wait client

                assertRequestsCount 1
      , testGroup "with 200 HTTP status from server" . List.singleton .
          Tasty.testCase "return a single successful HTTP response" .
            withServer application $ \port -> do
              assertRequestsCount 0

              UnliftIO.withAsync (sendHttpRequest port) $ \client -> do
                assertResponse Success =<< UnliftIO.wait client

                assertRequestsCount 1
        ]
  ]
  where
    assertRequestsCount
      :: HasField "requestsCounter" env (UnliftIO.MVar Word8)
      => Word8
      -> MIO env ()
    assertRequestsCount expected =
      liftIO . (@?= expected) =<< UnliftIO.readMVar =<< asks (.requestsCounter)

    assertResponse :: MonadIO m => ExpectedResponse -> HTTP.Result JSON.Value -> m ()
    assertResponse expected = liftIO . either mkAssertion assertSuccess
      where
        assertSuccess :: JSON.Value -> Tasty.Assertion
        assertSuccess value = case (expected, value) of
          (Success, JSON.Null) -> pure ()
          _                    -> mkError value

        mkAssertion :: HTTP.ResponseError -> Tasty.Assertion
        mkAssertion error = case (expected, error) of
          (ConnectionFailure, HTTP.HTTPError (HTTP.ConnectionFailure _))                   -> pure ()
          (ResponseTimeout,   HTTP.HTTPError HTTP.ResponseTimeout)                         -> pure ()
          (Status500,         HTTP.UnexpectedStatusCode status) | status == HTTP.status500 -> pure ()
          (_,                 _)                                                           -> mkError error

        mkError :: Show a => a -> Tasty.Assertion
        mkError received
          = Tasty.assertFailure
          $ "expected " <> show expected <> " but received " <> show received

    setRequest500 :: HTTP.Request -> HTTP.Request
    setRequest500 request =
      request
        { HTTP.path = "500"
        }

    setRequestTimeout :: HTTP.Request -> HTTP.Request
    setRequestTimeout request =
      request
        { HTTP.responseTimeout = HTTP.responseTimeoutMicro 1_000_000
        , HTTP.path            = "timeout"
        }

application :: WAI.Application
application req respond = do
  when (WAI.pathInfo req == ["timeout"])
    $ UnliftIO.threadDelay 2_000_000

  respond
    . WAI.responseLBS status [jsonHeader]
    $ JSON.encode JSON.Null
  where
    status :: HTTP.Status
    status = case WAI.pathInfo req of
      ["500"] -> HTTP.status500
      _       -> HTTP.status200

    jsonHeader :: HTTP.Header
    jsonHeader = (HTTP.hContentType, "application/json; charset=utf-8")

runServer
  :: MonadIO m
  => Warp.Port
  -> Network.Socket
  -> m ()
runServer port socket
  = liftIO
  $ Warp.runSettingsSocket settings socket application
  where
    settings :: Warp.Settings
    settings = Warp.setPort port Warp.defaultSettings

withServer
  :: WAI.Application
  -> (Warp.Port -> MIO Environment a)
  -> IO a
withServer app action =
  Warp.withApplication (pure app) (runWithEnvironment . action)

sendHttpRequest
  :: HTTP.Env env
  => Warp.Port
  -> MIO env (HTTP.Result JSON.Value)
sendHttpRequest port = sendHttpRequest' port identity identity

sendHttpRequest'
  :: HTTP.Env env
  => Warp.Port
  -> (HTTP.Request -> HTTP.Request)
  -> (HTTP.Transaction JSON.Value -> HTTP.Transaction JSON.Value)
  -> MIO env (HTTP.Result JSON.Value)
sendHttpRequest' port modifyRequest modifyTransaction
  = HTTP.send'
    (modifyTransaction HTTP.defaultTransaction)
    HTTP.decodeJSONOk
  $ modifyRequest request
  where
    request :: HTTP.Request
    request =
      HTTP.defaultRequest
        { HTTP.port = port
        }

runWithEnvironment :: MIO Environment a -> IO a
runWithEnvironment action = do
  requestsCounter <- UnliftIO.newMVar 0
  httpManager     <- HTTP.newManager $ managerSettings requestsCounter

  runMIO
    Environment
      { logAction       = Log.defaultCLIAction
      , httpSendRequest = (`HTTP.httpLbs` httpManager)
      , ..
      }
    action
  where
    managerSettings :: UnliftIO.MVar Word8 -> HTTP.ManagerSettings
    managerSettings counter =
      HTTP.defaultManagerSettings
        { HTTP.managerWrapException = managerWrapException
        }
      where
        -- Note: managerWrapException is fully called during successful responses and error exceptions
        -- while HTTP.httpLbs is not fully executed during exceptions (exits early)
        managerWrapException :: HTTP.Request -> IO a -> IO a
        managerWrapException request action' = do
          -- increments requests counter
          UnliftIO.modifyMVar_ counter (pure . succ)

          HTTP.managerWrapException HTTP.defaultManagerSettings request action'
